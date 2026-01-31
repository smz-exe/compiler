(* File lexer.mll *)
{
open Parser
exception No_such_symbol of string

let line_num = ref 1
let current_token = ref ""

let token t name =
    current_token := name;
    t
}

let digit = ['0'-'9']
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule lexer = parse
| digit+ as num           { token (NUM (int_of_string num)) num }
| "if"                    { token IF "if" }
| "else"                  { token ELSE "else" }
| "while"                 { token WHILE "while" }
| "scan"                  { token SCAN "scan"}
| "sprint"                { token SPRINT "sprint" }
| "iprint"                { token IPRINT "iprint" }
| "int"                   { token INT "int" }
| "return"                { token RETURN "return" }
| "type"                  { token TYPE "type" }
| "void"                  { token VOID "void" }
| id as text              { token (ID text) text }
| '\"'[^'\"']*'\"' as str { token (STR str) str }
| '='                     { token ASSIGN "=" }
| "=="                    { token EQ "==" }
| "!="                    { token NEQ "!=" }
| '>'                     { token GT ">" }
| '<'                     { token LT "<" }
| ">="                    { token GE ">=" }
| "<="                    { token LE "<=" }
| '+'                     { token PLUS "+" }
| '-'                     { token MINUS "-" }
| '*'                     { token TIMES "*" }
| '%'                     { token MOD "%" }
| "//"[^'\n']*            { lexer lexbuf } (* single-line comment *)
| '/'                     { token DIV "/" }
| '{'                     { token LB "{" }
| '}'                     { token RB "}" }
| '['                     { token LS "[" }
| ']'                     { token RS "]" }
| '('                     { token LP "(" }
| ')'                     { token RP ")" }
| ','                     { token COMMA "," }
| ';'                     { token SEMI ";" }
| [' ' '\t']              { lexer lexbuf } (* eat up whitespace *)
| '\n'                    { incr line_num; lexer lexbuf}
| eof                     { raise End_of_file }
| _  as c                 { raise (No_such_symbol (String.make 1 c)) }
