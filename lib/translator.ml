open Syntax
open Ast
open Analysis
open Optimizer
open Typecheck
open Mips
open Builtins
open Format

let tr_bop = function
  | Add -> add
  | Sub -> sub
  | Mul -> mul
  | Div -> div
  | Mod -> rem
  | And -> and_
  | Or -> or_
  | Xor -> xor_
  | Rol -> rol
  | Ror -> ror
  | Lt -> slt
  | Le -> sle
  | Gt -> sgt
  | Ge -> sge
  | Eq -> seq
  | Ne -> sne

let rec req_register = function
  | BOP (_, e1, e2) -> req_register e1 + req_register e2
  | _ -> 1

let tr_function type_env struct_env fdef =
  (*
  
    Here we instantiate the local hash table for the function.
    It shall store arguments and local variables of the function. 
  
  *)
  let env = Hashtbl.create 16 in
  (* let set_stack_data axe k (id, t) =
       let size = match t with Array s -> s | _ -> 1 in
       let offset = axe * size * (if axe = 1 then 4 * (k + 1) else 4 * (k + 2)) in
       Hashtbl.add env id offset;
     in *)
  List.iteri (fun k (_, id) -> Hashtbl.add env id (4 * (k + 1))) fdef.args;
  List.iteri (fun k (_, id) -> Hashtbl.add env id (-4 * (k + 2))) fdef.locals;

  List.iter (fun (t, id) -> Hashtbl.add type_env id t) fdef.locals;

  let lc = ref (-1) in
  let label_fmt () =
    lc := !lc + 1;
    "__" ^ fdef.name ^ "_" ^ string_of_int !lc
  in

  let rec tr_expr ri = function
    | Var id -> (
        match Hashtbl.find_opt env id with
        | Some offset -> lw t.(ri) offset fp
        | None -> la t.(ri) id @@ lw t.(ri) 0 t.(ri))
    | Val ty -> (
        match ty with
        | Int i -> li t.(ri) i
        | Char c ->
            let i = Char.code c in
            li t.(ri) i
        | Bool b -> if b then li t.(ri) 1 else li t.(ri) 0
        | String str -> tr_expr ri (Val (Array (str_to_array str)))
        | Array l ->
            (*
        Static array allocation (on the stack)
        It evaluates each given element then pushes it
        on the stack. It returns the address of the
        last pushed element.    
      *)
            let stack_list =
              List.fold_right
                (fun e code -> code @@ tr_expr ri e @@ push t.(ri))
                l nop
            in
            stack_list @@ move t.(ri) sp)
    | BOP (bop, e1, e2) ->
        (*
        Here is a way to optimize the
        assembly code in order to avoid
        any call to the stack.
        
        We call first the translate expression
        function on the expression which
        is likely to use the most registers.

        This way, on binary operators, we only use
        two register for arbitarily long expressions.

        !!
        Nevertheless there is a problem. Indeed, it breaks
        our semantic, especially for non-associative
        operators such as Lt.
      *)
        let alloc_a = req_register e1 in
        let alloc_b = req_register e2 in
        let op = tr_bop bop in
        let e1_, e2_ = if alloc_b > alloc_a then (e2, e1) else (e1, e2) in
        let eva = tr_expr ri e1_ in
        let evb = tr_expr (ri + 1) e2_ in
        (* (if alloc_b > alloc_a then tr_expr ri e2 @@ tr_expr (ri + 1) e1
           else tr_expr ri e1 @@ tr_expr (ri + 1) e2) @@ op t.(ri) t.(ri) t.(ri + 1) *)
        eva @@ evb
        @@
        if alloc_b > alloc_a then op t.(ri) t.(ri + 1) t.(ri)
        else op t.(ri) t.(ri) t.(ri + 1)
    | Ref id -> (
        match Hashtbl.find_opt env id with
        | Some offset -> addi t0 fp offset
        | None -> la t0 id)
    | Deref e -> tr_expr ri e @@ lw t.(ri) 0 t.(ri)
    | Call (id, args) ->
        (*
        
          Arguments are first placed in the a0, a1, a2, a3
          registers.

          If more, there then stay in t0 after evaluation and
          t0 is pushed on the stack.
        
        *)
        let args_code =
          List.fold_right
            (fun e code -> code @@ tr_expr ri e @@ push t0)
            args nop
        in
        args_code @@ jal id @@ addi sp sp (4 * List.length args)
    (* and store_args args =
       let rec aux i = function
         | [] -> nop
         | h :: t when i < 4 -> aux (i + 1) t @@ tr_expr h @@ move a.(i) t0
         | l -> List.fold_right (fun e code -> code @@ tr_expr e @@ push t0) l nop
       in
       aux 0 args *)
    | Alloc e ->
        tr_expr ri e @@ move a0 t.(ri) @@ li v0 9 @@ syscall @@ move t.(ri) v0
    | Read m -> tr_expr ri (Deref (tr_mem m))
    | New name ->
        (*
             
        *)
        let sct = Hashtbl.find struct_env name in
        let size = sizeof_struct sct in
        tr_expr ri (Alloc (Val (Int size)))
    | NewArray (_, e) -> tr_expr ri (Alloc e)
  (*
        To access memory, we are using the Read expression.
        We assume we can access memory with three different ways :
          - Raw : pointer arithmetic, direct access
          - Arr : implicit pointer arithmetic
          - Str : structure access to a field
          e.g.
            (a + 3)
            a[1 + 5]
            (a + 2)[1]
            p.x[1]
            p.q.x Str(Read(Str(Read(Str(Var "p", "q"))), "x")
      *)
  and tr_mem = function
    | Raw e -> e
    | Arr (source, offset) -> BOP (Add, source, BOP (Mul, Val (Int 4), offset))
    | Str (Var id, field) ->
        let t = Hashtbl.find type_env id in
        let def =
          match t with
          | TStruct name -> Hashtbl.find struct_env name
          | _ -> failwith "This is not a structure type!"
        in
        let offset = get_field_offset def field in
        tr_mem (Arr (Var id, Val (Int offset)))
        (* match sct_def_opt with
           | Some def ->

           | None -> failwith "That structure hasn't been defined!") *)
    | Str (Read m, _) -> tr_mem m
    | _ -> failwith "Illegal operation!"
  in

  let rec tr_seq = function
    | [] -> nop
    | [ x ] -> tr_stm x
    | h :: t -> tr_stm h @@ tr_seq t
  and tr_stm = function
    | Assign ld -> (
        let id, e = ld.cnt in
        match Hashtbl.find_opt env id with
        | Some offset -> tr_expr 0 e @@ sw t0 offset fp
        | None -> la t1 id @@ sw t1 0 t0)
    | If (ld, s1, s2) ->
        let then_label = label_fmt () in
        let end_label = label_fmt () in

        tr_expr 0 ld.cnt @@ bnez t0 then_label @@ tr_seq s2 @@ b end_label
        @@ label then_label @@ tr_seq s1 @@ label end_label
    | While (ld, s) ->
        let test_label = label_fmt () in
        let code_label = label_fmt () in

        b test_label @@ label code_label @@ tr_seq s @@ label test_label
        @@ tr_expr 0 ld.cnt @@ bnez t0 code_label
    | Return ld -> tr_expr 0 ld.cnt @@ subi sp fp 4 @@ pop ra @@ pop fp @@ jr ra
    | Expr ld -> tr_expr 0 ld.cnt
    | Write ld ->
        let d, e = ld.cnt in
        tr_expr 0 (tr_mem d)
        (* @@ push t0 *)
        @@ tr_expr 1 e
        (* @@ pop t1 *)
        @@ sw t1 0 t0
  in
  push fp @@ push ra @@ addi fp sp 4
  @@ subi sp sp (4 * List.length fdef.locals)
  @@ tr_seq fdef.code @@ li t0 0 @@ subi sp fp 4 @@ pop ra @@ pop fp @@ jr ra

(*
  The head sequence is the very first sequence.
  It shall push the amount of arguments and the arguments
  on the stack and then call the main function.   
*)

let head = push a0 @@ jal "main" @@ li v0 10 @@ syscall

module Translator = struct
  let translate f =
    Printf.printf "Translating %s\n" f;
    let prog = parse f in

    (*
      Now the code is parsed, there is some optimizations
      to do, producing a new AST which will be sent
      to the translating process.
    *)
    let type_env = Hashtbl.create 16 in
    List.iter (fun (t, id) -> Hashtbl.add type_env id t) prog.globals;

    let function_env = Hashtbl.create 16 in
    List.iter
      (fun (f : fun_def) -> Hashtbl.add function_env f.name f)
      (__builtin_print_char_def :: __builtin_print_int_def :: prog.functions);

    let struct_env = Hashtbl.create 16 in
    List.iter
      (fun (s : struct_def) -> Hashtbl.add struct_env s.name s)
      prog.structs;

    let env =
      { typing = type_env; functions = function_env; structs = struct_env }
    in

    Printf.printf "Type-check the code\n";
    List.iter
      (fun (f : fun_def) ->
        let wc = tp_fun f env in
        if wc then () else failwith "Typecheck error!")
      prog.functions;

    Printf.printf "Dataflow analysis\n";

    List.iter
      (fun f ->
        let s = f.code in
        let b = blocks_of s in
        let df = dataflow b in
        print_dataflow df;
        Printf.printf "Done\n";
        let rd = deadcode_reduction s df in
        Printf.printf "%s\n" (show_seq rd))
      prog.functions;

    Printf.printf "Generating assembly code\n";

    let fn_asm =
      List.fold_right
        (fun (def : Ast.fun_def) code ->
          label def.name @@ tr_function type_env struct_env def @@ code)
        prog.functions nop
    in
    let text = head @@ fn_asm @@ __builtin_print_int @@ __builtin_print_char in
    let data =
      List.fold_right
        (fun (_, id) code -> label id @@ dword [ 0 ] @@ code)
        prog.globals nop
    in
    let prog = { text; data } in
    let output_fn = Filename.chop_suffix f ".imp" ^ ".asm" in
    let out = open_out output_fn in
    let fmt = formatter_of_out_channel out in
    Mips.print_program fmt prog;
    pp_print_flush fmt ();
    close_out out
end
