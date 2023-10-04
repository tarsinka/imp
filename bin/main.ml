open Imp.Translator

let inputs = ref []
let output = ref ""
let spec = [ ("-o", Arg.Set_string output, "Output file") ]
let handler fn = inputs := fn :: !inputs
let usage = "imp <file> -o <output>"
let () = Arg.parse spec handler usage;
  List.iter (fun f -> Translator.translate f) !inputs
