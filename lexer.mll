{
  open Printf
  open Parser
  exception END_OF_FILE
  (*Defined all tokens used*)
  (*INDICES FUNC (PLUS, MINUS, TIMES, DIV, COUNT, COLCOUNT, ROWCOUNT, SUM, COLSUM, ROWSUM, AVG, ROWAVG, COLAVG, MIN, ROWMIN, COLMIN, MAX, ROWMAX, COLMAX)*)
  (*SPACE, NEWLINE, LPAREN, RPAREN, LBRAC, RBRAC, HYPHEN, COMMA, SEMICOLON, ASSIGNMENT*)
  (*type token =
    | INDICES | RANGE | COLON | LPAREN | RPAREN | LBRAC | RBRAC | COMMA | SEMICOLON | ASSIGNMENT 
    | COUNT | COLCOUNT | ROWCOUNT 
    | SUM | ROWSUM | COLSUM 
    | AVG | ROWAVG | COLAVG 
    | MIN | ROWMIN | COLMIN 
    | MAX | COLMAX | ROWMAX
    | PLUS | MINUS | TIMES | DIV   
    | INT of int | FLOAT of float | ERR
    | EOF*)
}

(*space or tab*)
let whitespace = [' ' '\t']
(*newline *)
let newline = ['\n']
(*positive numbers single digit*)
let digit = ['0' - '9']
(*negative and positive integers icluding zero*)
let integer =  "-"?(['1'-'9']['0'-'9']*)|'0'
(*string of form [0-3]*)
let indices = "["['0'-'9']','['0'-'9']"]"
(*range of form ([0-3],[0,2]) : ([1-2],[1-3])*)
let range = "(["['0'-'9']','['0'-'9']"] : ["['0'-'9']','['0'-'9']"])"

(*Lex Rules*)
rule token = parse
    | whitespace                                                             										{SPACE}
    | newline                                                                										{NEWLINE}
    | integer as num1                                                        										{INT(int_of_string num1)}
    | (integer + ('.'(digit+))) as num2                                        										{FLOAT(float_of_string num2)} 
    | "["((integer) as num1) ',' ((integer) as num2)"]"                                              				{INDICES((int_of_string num1), (int_of_string num2))}
    | "(["((integer) as num1) ',' ((integer) as num2)"] : ["((integer) as num3) ',' ((integer) as num4)"])"         {RANGE((int_of_string num1),(int_of_string num2),(int_of_string num3),(int_of_string num4))}  
    | ';'                                                                    										{SEMICOLON}
    | ":="                                                                   										{ASSIGNMENT}                    
    | "ADD" as str                                                                  								{ADD(str)}
    | "SUBT" as str                                                                 								{SUBT(str)}
    | "MULT" as str                                                                 								{MULT(str)}
    | "DIV" as str                                                                  								{DIV(str)}
    | "COUNT" as str                                                                								{COUNT(str)}
    | "ROWCOUNT" as str                                                             								{ROWCOUNT(str)}
    | "COLCOUNT" as str                                                             								{COLCOUNT(str)}
    | "SUM" as str                                                                  								{SUM(str)}
    | "ROWSUM" as str                                                               								{ROWSUM(str)}
    | "COLSUM" as str                                                               								{COLSUM(str)}
    | "AVG" as str                                                                  								{AVG(str)}
    | "ROWAVG" as str                                                               								{ROWAVG(str)}
    | "COLAVG" as str                                                               								{COLAVG(str)}
    | "MIN" as str                                                                  								{MIN(str)}
    | "ROWMIN" as str                                                               								{ROWMIN(str)}
    | "COLMIN" as str                                                               								{COLMIN(str)}
    | "MAX" as str                                                                  								{MAX(str)}
    | "ROWMAX" as str                                                               								{ROWMAX(str)}
    | "COLMAX" as str                                                               								{COLMAX(str)}
    | eof                                                                            								{raise END_OF_FILE}
    |_                                                                              								{ERR}                   (*Anything else will be treated as error*)