let parse file =
  let input = open_in file in
  let lexbuf = Lexing.from_channel input in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = file };
  let ast = try Parser.prog Lexer.token lexbuf with
  | Lexer.SyntaxError e ->
    Printf.printf "%s\n" e;
    { globals = []; functions = [] ; structs = [] }
    | Parser.Error ->
      Printf.printf "Parser error\n";
      { globals = []; functions = [] ; structs = [] }
    in 
    close_in input;
    ast