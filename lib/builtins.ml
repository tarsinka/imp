open Ast
open Mips

let __beg = push fp @@ push ra @@ addi fp sp 4
let __end = subi sp fp 4 @@ pop ra @@ pop fp @@ jr ra

let __builtin_print_char_def =
  {
    name = "print_char";
    args = [ (TChar, "c") ];
    locals = [];
    code = [];
    return = TVoid;
  }

let __builtin_print_char =
  label "print_char" @@ __beg @@ lw a0 4 fp @@ li v0 11 @@ syscall @@ __end

let __builtin_print_int_def =
  {
    name = "print_int";
    args = [ (TInt, "i") ];
    locals = [];
    code = [];
    return = TVoid;
  }

let __builtin_print_int =
  label "print_int" @@ __beg @@ lw a0 4 fp @@ li v0 1 @@ syscall @@ __end
