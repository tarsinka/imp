{
        open Parser
        exception SyntaxError of string
}

let white = ' ' | '\t' | '\r' | '\n' | "\r\n"
let digit = ['0'-'9']
let int = '-'? digit digit*
let char = '\''(['!'-'~'] | white | digit)'\''
let string = '"'(['!'-'~'] | white | digit)*'"'

let var = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule token = parse
        | white+        { token lexbuf } 
        | "true"        { TRUE }
        | "false"       { FALSE }
        | '+'           { ADD }
        | '-'           { SUB }
        | '*'           { MUL }
        | '/'           { DIV }
        | '%'           { MOD }
        | '&'           { AND }
        | '|'           { OR }
        | '^'           { XOR }
        | "<<"          { ROL }
        | ">>"          { ROR }
        | '<'           { LT }
        | "<="          { LE }
        | '>'           { GT }
        | ">="          { GE }
        | "=="          { EQ }
        | '='           { ASSIGN }
        | ';'           { SEMI_COL }
        | ','           { COMMA }
        | '.'           { DOT }
        | '{'           { BEGIN }
        | '}'           { END }
        | '('           { LPAR }
        | ')'           { RPAR }
        | '['           { LARRAY }
        | ']'           { RARRAY }
        | "var"         { VAR }
        | "struct"      { STRUCT }
        | "new"         { NEW }
        | "function"    { FUNCTION }
        | "return"      { RETURN }
        | "if"          { IF }
        | "else"        { ELSE }
        | "while"       { WHILE }
        | "void"        { TY_VOID }
        | "int"         { TY_INT }
        | "char"        { TY_CHAR }
        | "bool"        { TY_BOOL }
        | "string"      { TY_STRING }
        | char          { CHAR(Lexing.lexeme_char lexbuf 1) }
        | string        { STRING(Lexing.lexeme lexbuf) }
        | var           { VARNAME(Lexing.lexeme lexbuf) }
        | int           { INT(int_of_string(Lexing.lexeme lexbuf)) } 
        | eof           { EOF }
        | _             { raise (SyntaxError ("Caractère inconnu : " ^ Lexing.lexeme lexbuf)) } 
