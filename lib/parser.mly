%{
        open Ast

        let lb = ref 0
        let get_next_label () = lb := !lb + 1; !lb

        let vars = ref []
        let structs = ref []
        let functions = ref []

%}
%token VAR STRUCT FUNCTION
%token<string> VARNAME
%token<int> INT
%token<char> CHAR
%token<string> STRING
%token TRUE FALSE

%token ADD SUB MUL DIV MOD
%token AND OR XOR ROL ROR
%token LT LE GT GE EQ NE
%token NEW

%token ASSIGN IF ELSE WHILE RETURN
%token COLUMN SEMI_COL BEGIN END LPAR RPAR LARRAY RARRAY COMMA DOT

(* Types *)

%token TY_INT TY_CHAR TY_BOOL TY_STRING TY_VOID
%token EOF

%left ADD SUB MUL DIV MOD AND OR XOR ROL ROR LT LE GT GE EQ NE
%nonassoc LARRAY DOT

%start<Ast.program> prog

%%
%inline binary_op:
        | ADD   { Add }
        | SUB   { Sub }
        | MUL   { Mul }
        | DIV   { Div }
        | MOD   { Mod }
        | AND   { And }
        | OR    { Or }
        | XOR   { Xor }
        | ROL   { Rol }
        | ROR   { Ror }
        | LT    { Lt }
        | LE    { Le }
        | GT    { Gt }
        | GE    { Ge }
        | EQ    { Eq }
        | NE    { Ne }

expr:
        | s = VARNAME { Var s }
        | i = INT { Val (Int i) }
        | TRUE { Val (Bool true) }
        | FALSE { Val (Bool false) }
        | c = CHAR { Val (Char c) }
        | s = STRING { Val(String s) }
        | LARRAY ; l = separated_list(COMMA, expr) ; RARRAY { Val (Array l) }
        | AND ; s = VARNAME { Ref s }
        | fname = VARNAME ; LPAR ; args=separated_list(COMMA, expr) ; RPAR { Call (fname, args) }
        | m=mem { Read m }
        | e=expr ; DOT ; fname=VARNAME ; LPAR ; args=separated_list(COMMA, expr) ; RPAR { MCall(e, fname, args) }
        | NEW ; s = VARNAME ; LPAR ; args=separated_list(COMMA, expr) ; RPAR { New (s, args) }
        | NEW ; LARRAY ; t=typ ; COMMA ; e=expr ; RARRAY { NewArray(t, e) }
        | x = expr ; op=binary_op ; y = expr { BOP (op, x, y) }
        
mem:
        | MUL ; LPAR ; e = expr ; RPAR { Raw e }
        | base = expr ; LARRAY ; offset = expr ; RARRAY { Arr(base, offset) }
        | e = expr ; DOT id = VARNAME { Str(e, id) }

stm:
        | v = VARNAME ; ASSIGN ; e = expr  SEMI_COL { Assign {cnt = (v, e) ; l = get_next_label ()} }
        | IF ; b = expr ; BEGIN ; s_1 = list(stm) ; END ; ELSE ; BEGIN ; s_2 = list(stm) ; END { If ({cnt = b ; l = get_next_label ()}, s_1, s_2) }
        | WHILE ; b = expr ; BEGIN ; s = list(stm) ; END { While ({cnt = b ; l = get_next_label ()}, s) } 
        | RETURN ; e = expr SEMI_COL { Return {cnt = e ; l = get_next_label ()} }
        | e = expr SEMI_COL { Expr {cnt = e ; l = get_next_label ()} }
        | m = mem ; ASSIGN ; e = expr SEMI_COL { Write {cnt = (m, e) ; l = get_next_label ()} } 

vars:
        VAR ; t=typ ; v = VARNAME ; SEMI_COL { (t, v) }

typed_id:
        t=typ ; v = VARNAME { (t, v) }

functions: 
        FUNCTION ; t=typ ; name = VARNAME ; LPAR ; args = separated_list(COMMA, typed_id) ; RPAR ; 
        BEGIN ; locals=list(vars) ; seq=list(stm) ; END { { name=name ; args=args ; code=seq ; locals=locals ; return=t } }

structs:
        STRUCT ; name = VARNAME ; parent = option(COLUMN ; p = VARNAME { p }) ; 
        BEGIN ; fields=list(vars) ; methods=list(functions) ; END { { name ; fields ; methods ; parent } }

typ:
        | TY_VOID       { TVoid }
        | TY_INT        { TInt }
        | TY_CHAR       { TChar }
        | TY_BOOL       { TBool }
        | TY_STRING     { TString }
        | t=typ ; MUL   { TPointer t }
        | LARRAY ; t=typ ; RARRAY { TArray t }
        | id=VARNAME { TStruct id }

declaration:
        | s = structs   { structs := s :: !structs }
        | v = vars      { let (t, id) = v in vars := (t, id) :: !vars }
        | f = functions { functions := f :: !functions }

prog:
        list(declaration) EOF { { globals=List.rev !vars ; functions=List.rev !functions ; structs=List.rev !structs } }
%%