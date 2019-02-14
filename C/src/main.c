#include <system.h>
#include <expression-parser.h>


int fgetc( Byte * filestream );
int feof ( Byte * filestream );


Boolean is_number_character(N_32 character)
{
    return character >= '0' && character <= '9';
}


Boolean is_latin_character(N_32 character)
{
    return character >= 'a' && character <= 'z' || character >= 'A' && character <= 'Z';
}

Boolean is_cyrillic_character(N_32 character)
{
    return character >= 0x0410 && character <= 0x044F || character == 0x0451 || character == 0x0401;
}


void write_UTF_8_character_in_buffer (Buffer* buffer, N_32 character)
{
    cycle(0, 4, 1)
    write_in_buffer(buffer, ((N_8*)&character)[i]);
    end
}


N_32 read_token (Buffer* token, Input* input)
{
    N_32 character;

    token->length = 0;

    if(end_of_input(input))
        goto error;

    character = input_UTF_8_data(input);//read_UTF_8_character(input);

    if(!is_latin_character(character) && !is_cyrillic_character(character) && character != '_')
        goto error;

    read_UTF_8_character(input);

    //printf("%d ", character);
    write_UTF_8_character_in_buffer(token, character);

    for(;;)
    {
        character = input_UTF_8_data(input);

        if(!is_latin_character(character) && !is_cyrillic_character(character) && !is_number_character(character) && character != '_')
            break;

        character = read_UTF_8_character(input);

        //printf("%d ", character);
        write_UTF_8_character_in_buffer(token, character);

        if(end_of_input(input))
            break;
    }

    return 1;

error:
    return 0;
}


N_32 read_next_UTF_8_character_from_string(N_8** string)
{
    N_8  current_byte;
    N_32 number_of_bytes;
    N_32 result;

    current_byte = **string;
    ++*string;

    if(!(current_byte & 0b10000000))
        return current_byte;

    if((current_byte & 0b11110000) == 0b11110000)
    {
        number_of_bytes = 4;
        result = (current_byte & 0b00001111) << 18;
    }
    else if((current_byte & 0b11100000) == 0b11100000)
    {
        number_of_bytes = 3;
        result = (current_byte & 0b00011111) << 12;
    }
    else if((current_byte & 0b11000000) == 0b11000000)
    {
        number_of_bytes = 2;
        result = (current_byte & 0b00111111) << 6;
    }
    else
        goto error;

    cycle(0, number_of_bytes - 1, 1)
    current_byte = **string;
    ++*string;
    result |= (current_byte & 0b00111111) << ((number_of_bytes - 2 - i) * 6);
    end

    return result;

error:
    return 1;
}


Z_32 compare_token(Buffer* token, N_8* string)
{
    N_32* token_data;
    N_32  token_length;
    Z_32  result;

    token_data = token->data;
    token_length = token->length / 4;

    for(; token_length && *string; --token_length, ++token_data)
    {
        result = *token_data - read_next_UTF_8_character_from_string(&string);

        if(result)
            return result;
    }

    if(*string)
        return read_next_UTF_8_character_from_string(&string);
    else if(token_length)
        return *token_data;

    return 0;
}


void copy_token(Buffer* destination, Buffer* source)
{
    destination->length = 0;

    cycle(0, source->length, 1)
        write_in_buffer(destination, source->data[i]);
    end
}


void print_token(Buffer* token)
{
    N_32* token_data;
    N_32  token_length;

    token_data = token->data;
    token_length = token->length / 4;

    cycle(0, token_length, 1)
        printf("%c", token_data[i]);
    end
}


typedef enum
{
    NUMBER,
    VARIABLE,
    SEQUENCE,
    LABEL,
    ARRAY_INDEX_EXPRESSION,

    ALLOCATE_ARRAY
}
Operand_Type;


typedef struct
{
    N_8  type;
    N_8* data;
}
Operand_Node;


Operand_Node* create_operand(N_8 type, N_8* data)
{
    Operand_Node* node;

    node = malloc(sizeof(Operand_Node));
    node->type = type;
    node->data = data;

    return node;
}


typedef struct
{
    //N_32 last_character;
    Input* input;
    Buffer token;
    Buffer operands;
}
Parser;

void print_expression_in_postfix_notation (Buffer* expression, void (*print_operand)(Byte* operand))
{
    Expression_Node* nodes;
    N_32             length;

    nodes = expression->data;
    length = expression->length / sizeof(Expression_Node);

    cycle(0, length, 1)
        Expression_Node* current_node = nodes + i;

        if(current_node->type == OPERAND)
        {
            Operand_Node* operand = current_node->data;

            if(operand != 1)
            {
                switch(operand->type)
                {
                case NUMBER:
                    printf("%d ", operand->data);
                    break;

                case SEQUENCE:
                    {
                        Buffer* sequence = operand->data;
                        N_32 sequence_length = sequence->length / sizeof(Operand_Node*);
                        Operand_Node* sequence_data = sequence->data;

                        cycle(0, sequence_length, 1)
                            switch(sequence_data[i].type)
                            {
                            case VARIABLE:
                                print_token(sequence_data[i].data);
                                printf(" ");
                                break;

                            case ARRAY_INDEX_EXPRESSION:
                                printf("[");
                                print_expression_in_postfix_notation(sequence_data[i].data, &print_operand);
                                printf("]");
                                break;
                            }
                        end
                        //printf("var");
                    }
                    break;

                case LABEL:
                    print_token(operand->data);
                    printf(":");
                    break;

                case ALLOCATE_ARRAY:
                    printf("[");
                    Buffer* array = operand->data;
                    N_32 array_length = array->length / sizeof(Buffer*);
                    Buffer** array_data = array->data;
                    cycle(0, array_length, 1)
                        print_expression_in_postfix_notation(array_data[i], &print_operand);
                        printf(",");
                    end
                    printf("]");
                    break;
                }
            }

            //print_operand(current_node->data);
            //printf("%c ", current_node->data);
        }

        if(current_node->type == BINARY_OPERATION)
            printf("%s ", ((Operation*)current_node->data)->name);
    end
}


void add_operand_in_stack (Buffer* stack, Operand_Node* operand)
{
    cycle(0, sizeof(Operand_Node*), 1)
        write_in_buffer(stack, ((Byte*)&operand)[i]);
    end
}


Operand_Node* remove_operand_from_stack (Buffer* stack)
{
    stack->length -= sizeof(Operand_Node*);
    return *(Operand_Node**)(stack->data + stack->length);
}

/*
void calculate_addresses (Buffer* expressions)
{
    Buffer** expressions_data;
    N_32     expressions_length;
    N_32     current_address;
    Buffer   stack;

    initialize_buffer(&stack, 100);
    current_address = 0;
    expressions_data = expressions->data;
    expressions_length = expressions->length / sizeof(Buffer*);

    cycle(0, expressions_length, 1)
        Buffer* expression = expressions_data[i];
        Expression_Node* expression_data = expression->data;
        N_32 expression_length = expression->length / sizeof(Expression_Node);

        cycle(0, expression_length, 1)
            Expression_Node* current_node = expression_data + i;

            switch(current_node->type)
            {
            case OPERAND:
                {
                    add_operand_in_stack(&stack, current_node->data);
                }
                break;

            case BINARY_OPERATION:
                {
                    Operation* operation = current_node->data;
                    //printf("%s ", operation->name);

                    if(!strcmp(operation->name, "="))
                    {
                        Operand_Node* operand2 = remove_operand_from_stack(&stack);
                        Operand_Node* operand1 = remove_operand_from_stack(&stack);

                        switch(operand2->type)
                        {
                        case NUMBER:
                            printf("push %d\n", operand2->data);
                            break;

                        case SEQUENCE:
                            {
                                Buffer* sequence = operand2->data;
                                N_32 sequence_length = sequence->length / sizeof(Operand_Node*);
                                Operand_Node* sequence_data = sequence->data;

                                cycle(0, sequence_length, 1)
                                    switch(sequence_data[i].type)
                                    {
                                    case VARIABLE:
                                        printf("push ");
                                        print_token(sequence_data[i].data);
                                        printf("\n");
                                        break;

                                    case ARRAY_INDEX_EXPRESSION:
                                        //printf("[");
                                        //print_expression_in_postfix_notation(sequence_data[i].data, &print_operand);
                                        //printf("]");
                                        break;
                                    }
                                end
                                //printf("var");
                            }
                            break;

                        case LABEL:
                            printf("error: expected operand after =\n");
                            //printf("push address\n");
                            //printf("%d\n", current_address);
                            //print_token(operand->data);
                            //printf(":");
                            break;

                        case ALLOCATE_ARRAY:
                            {
                            //printf("[");
                            Buffer* array = operand2->data;
                            N_32 array_length = array->length / sizeof(Buffer*);
                            //Buffer** array_data = array->data;
                            //cycle(0, array_length, 1)
                            //    print_expression_in_postfix_notation(array_data[i], &print_operand);
                            //    printf(",");
                            //end
                            //printf("]");
                            current_address += array_length;
                            }
                            break;
                        }

                        current_address += 10;
                        //printf("mov");
                    }
                }
                break;

            case UNARY_OPERATION:
                break;
            }
        end
    end
}
*/


typedef struct
{
    Buffer* expression;
    N_32    index;
}
Body_Node;


void calculate_macro_addresses (Buffer* expressions)
{
    Body_Node* expressions_data;
    N_32       expressions_length;
    N_32       current_address;
    Buffer     stack;

    initialize_buffer(&stack, 100);
    current_address = 0;
    expressions_data = expressions->data;
    expressions_length = expressions->length / sizeof(Body_Node);

    cycle(0, expressions_length, 1)
        Body_Node* body_node = expressions_data + i;
        body_node->index = i;
        Buffer* expression = body_node->expression;
        Expression_Node* expression_data = expression->data;
        N_32 expression_length = expression->length / sizeof(Expression_Node);

        cycle(0, expression_length, 1)
            Expression_Node* current_node = expression_data + i;

            switch(current_node->type)
            {
            case OPERAND:
                {
                    add_operand_in_stack(&stack, current_node->data);
                }
                break;

            case BINARY_OPERATION:
                {
                    Operation* operation = current_node->data;
                    //printf("%s ", operation->name);

                    if(!strcmp(operation->name, "="))
                    {
                        Operand_Node* operand2 = remove_operand_from_stack(&stack);
                        Operand_Node* operand1 = remove_operand_from_stack(&stack);

                        switch(operand2->type)
                        {
                        case NUMBER:
                            printf("push %d\n", operand2->data);
                            break;

                        case SEQUENCE:
                            {
                                Buffer* sequence = operand2->data;
                                N_32 sequence_length = sequence->length / sizeof(Operand_Node*);
                                Operand_Node* sequence_data = sequence->data;

                                cycle(0, sequence_length, 1)
                                    switch(sequence_data[i].type)
                                    {
                                    case VARIABLE:
                                        printf("push ");
                                        print_token(sequence_data[i].data);
                                        printf("\n");
                                        break;

                                    case ARRAY_INDEX_EXPRESSION:
                                        //printf("[");
                                        //print_expression_in_postfix_notation(sequence_data[i].data, &print_operand);
                                        //printf("]");
                                        break;
                                    }
                                end
                                //printf("var");
                            }
                            break;

                        case LABEL:
                            printf("error: expected operand after =\n");
                            //printf("push address\n");
                            //printf("%d\n", current_address);
                            //print_token(operand->data);
                            //printf(":");
                            break;

                        case ALLOCATE_ARRAY:
                            {
                            //printf("[");
                            Buffer* array = operand2->data;
                            N_32 array_length = array->length / sizeof(Buffer*);
                            //Buffer** array_data = array->data;
                            //cycle(0, array_length, 1)
                            //    print_expression_in_postfix_notation(array_data[i], &print_operand);
                            //    printf(",");
                            //end
                            //printf("]");
                            current_address += array_length;
                            }
                            break;
                        }

                        current_address += 10;
                        //printf("mov");
                    }
                }
                break;

            case UNARY_OPERATION:
                break;
            }
        end
    end
}


void print_operand (char operand)
{

    printf("%c", operand);
}


Buffer* read_expression (Parser* parser);
Buffer* parse_left_operand (Parser* parser, N_32 operand_offset);


Operand_Node* parse_expression_operand(Parser* parser)
{
    N_8           operand_type;
    Operand_Node* node;

    node = 1;

    read_token(&parser->token, parser->input);
    skip_spaces(parser->input);

    if(parser->token.length)
    {
        if(input_UTF_8_data(parser->input) == ':')
        {
            //printf("label ");
            //print_token(parser);
            Buffer* new_token = malloc(sizeof(Buffer));
            initialize_buffer(new_token, 10);
            copy_token(new_token, &parser->token);
            node = create_operand(LABEL, new_token);

            read_UTF_8_character(parser->input);
            skip_spaces(parser->input);
        }
        else
        {
            Buffer* sequence = parse_left_operand(parser, -1);

            if(sequence)
                node = create_operand(SEQUENCE, sequence);
        }
    }
    else
    {
        operand_type = input_data(parser->input);

        if(is_number_character(operand_type))
        {
            N_32 number = read_N_32(parser->input);
            node = create_operand(NUMBER, number);
            //printf("%d, ", read_N_32(parser->input));
        }
        else
        {
            switch(operand_type)
            {
            case '[':
                read_input(parser->input);
                skip_spaces(parser->input);

                Buffer* array = malloc(sizeof(Buffer));
                initialize_buffer(array, 20);

                while(!end_of_input(parser->input) && input_UTF_8_data(parser->input) != ']')
                {
                    Buffer* expression = read_expression(parser);
                    cycle(0, sizeof(Buffer*), 1)
                        write_in_buffer(array, ((Byte*)&expression)[i]);
                    end
                }

                if(input_data(parser->input) == ']')
                    read_UTF_8_character(parser->input);
                else
                    printf("error: expected ]\n");

                node = create_operand(ALLOCATE_ARRAY, array);
                //printf("allocate array expression, ");
                break;

            case '{':
                read_input(parser->input);
                //printf("function declaration, ");
                break;

            case '"':
                read_input(parser->input);
                //printf("const string, ");
                break;
            }
        }

        skip_spaces(parser->input);
    }

    return node;

error:
    return 0;
}


Operation operations[] = {
    {BINARY_OPERATION, ">=", 1},
    {BINARY_OPERATION, "*", 1},
    {BINARY_OPERATION, "+", 2},
    {BINARY_OPERATION, "=", 8},
    {BINARY_OPERATION, "if", 9},
    {UNARY_OPERATION, "-", 0},
    {UNARY_OPERATION, "length", 0}
};


Buffer* read_expression (Parser* parser)
{
    Buffer* expression;
    N_32 status;

    expression = malloc(sizeof(Buffer));

    status = parse_expression (
        expression,
        parser->input,
        operations,
        sizeof(operations) / sizeof(Operation),
        parser,
        &parse_expression_operand);

    if(!status)
    {
        printf("error\n");
        return 0;
    }

    return expression;
}

/*
N_32 allocate_operand(Parser* parser)
{
    N_32 new_operand_offset;

    new_operand_offset = parser->operands.length;

    cycle(0, sizeof(Buffer), 1)
        write_in_buffer(&parser->operands, 0);
    end

    initialize_buffer(parser->operands.data + new_operand_offset, 20);

    return new_operand_offset;
}


Buffer* get_operand(Parser* parser, N_32 operand_offset)
{
    return parser->operands.data + operand_offset;
}

/*
typedef enum
{
    VARIABLE
}
Operand_Node_Type;


typedef struct
{
    Operand_Node_Type type;
    Byte*             data;
}
Operand_Node;


void add_in_operand (Parser* parser, N_32 operand_offset, Operand_Node_Type type)
{

}
*/

void add_operand_variable (Parser* parser, N_32 operand_offset, Buffer* variable_name)
{
    printf("add variable, ");
}


Buffer* parse_left_operand (Parser* parser, N_32 operand_offset)
{
    Buffer* operand = malloc(sizeof(Buffer));
    initialize_buffer(operand, 20);

    //operand = get_operand(parser, operand_offset);

    if(!parser->token.length)
    {
        printf("not operand\n");
        goto error;
    }

    //add_operand_variable(parser, operand_offset, &parser->token);
    Buffer* new_token = malloc(sizeof(Buffer));
    initialize_buffer(new_token, 2);
    copy_token(new_token, &parser->token);
    Operand_Node* node = create_operand(VARIABLE, new_token);
    cycle(0, sizeof(Operand_Node), 1)
        write_in_buffer(operand, ((Byte*)node)[i]);
    end

    skip_spaces(parser->input);

read_operand:

    switch(input_UTF_8_data(parser->input))
    {
    case '[':
        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);
        //printf("array index, ");

        Buffer* expression = read_expression(parser);
        Operand_Node* node = create_operand(ARRAY_INDEX_EXPRESSION, expression);
        cycle(0, sizeof(Operand_Node), 1)
            write_in_buffer(operand, ((Byte*)node)[i]);
        end

        if(input_UTF_8_data(parser->input) != ']')
        {
            printf("error: expected ]\n");
            goto error;
        }

        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);

        goto read_operand;
        break;

    case '(':
        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);
        //printf("function call, ");

        while(!end_of_input(parser->input))
        {
            if(input_UTF_8_data(parser->input) == ')')
                break;

            read_expression(parser);

            if(input_UTF_8_data(parser->input) == ',')
                read_UTF_8_character(parser->input);

            skip_spaces(parser->input);
        }

        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);

        goto read_operand;
        break;

    case '.':
        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);
        //printf("system function call, ");

        read_token(&parser->token, parser->input);
        skip_spaces(parser->input);

        if(input_UTF_8_data(parser->input) != '(')
            printf("error: expected (\n");

        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);

        while(!end_of_input(parser->input))
        {
            if(input_UTF_8_data(parser->input) == ')')
                break;

            read_expression(parser);

            if(input_UTF_8_data(parser->input) == ',')
                read_UTF_8_character(parser->input);

            skip_spaces(parser->input);
        }

        read_UTF_8_character(parser->input);
        skip_spaces(parser->input);

        goto read_operand;
        break;
    }

    return operand;

error:
    return 0;
}


N_8 parse (Input* input)
{
    Parser parser;
    Buffer expressions;

    initialize_buffer(&expressions, 100);

    parser.input = input;
    initialize_buffer(&parser.token, 20);
    initialize_buffer(&parser.operands, 20);

    while(!end_of_input(input))
    {
        Buffer* expression = read_expression(&parser);
        Body_Node node;

        node.expression = expression;
        //print_expression_in_postfix_notation(expression, &print_operand);
        //printf("\n");

        cycle(0, sizeof(Body_Node), 1)
            write_in_buffer(&expressions, ((Byte*)&node)[i]);
        end

        /*
        read_token(&parser.token, input);

        if(!compare_token(&parser.token, "если"))
        {
            printf("if\n");
        }
        else
        {
            N_32 operand_offset = allocate_operand(&parser);
            if(!parse_left_operand(&parser, operand_offset))
            {
                printf("error parse operand\n");
                goto error;
            }

            if(input_UTF_8_data(input) == '=')
            {
                printf("ASSIGNMENT ");

                read_UTF_8_character(input);
                skip_spaces(input);
                read_expression(&parser);
                printf("\n");
            }
            else
            {
                printf("\n");
            }
        }*/

        //skip_spaces(input);
    }

    //calculate_addresses(&expressions);
    calculate_macro_addresses(&expressions);

    return 1;

error:
    return 0;
}


N_32 main()
{
    Input   input;
    Byte*   input_file;
    Buffer  expression;
    N_32    is_success;
    Buffer  token;

    Parser parser;

    input_file = fopen("test", "rb");
    initialize_input(&input, input_file, &fgetc);
    input.end_of_data = &feof;

    parse(&input);
    /*
        initialize_buffer(&token, 20);

        read_token(&token, &input);


        char* utf_string = "абв";

        //while(*utf_string)
        //    printf("%d ", read_next_UTF_8_character_from_string(&utf_string));

        printf("%d", compare_token(&token, utf_string));
    */
    return 0;
}
