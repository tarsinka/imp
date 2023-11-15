open Syntax
open Ast
open Analysis
open Optimizer
open Typecheck
open Mips
open Builtins
open Format

type function_kind = Function | Method
type memory_mapper = Reg of int * register | Offset of int

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

let desc_fmt sct_name = "__" ^ sct_name ^ "_"

let rec tr_function gl_env _ fdef fun_kind =
  (*
  
    Here we instantiate the local hash table for the function.
    It shall store arguments and local variables of the function. 
  
  *)
  let env = Hashtbl.create 16 in

  (* let ri_offset = 1 + Hashtbl.fold (fun _ v i -> if v > i then v else i) regdist 0 in *)
  let ri_offset = 0 in
  Printf.printf "RI OFF %d\n" ri_offset;

  List.iteri
    (fun k (_, id) ->
      let os = 4 * (k + 1) in
      let mm = if k < 0 then Reg (os, a.(k)) else Offset os in
      Hashtbl.add env id mm)
    fdef.args;
  List.iteri
    (fun k (_, id) ->
      (* let ri = Hashtbl.find regdist id in *)
      let ri = 8 in
      let os = -4 * (k + 2) in
      let slot = if ri > 7 then Offset os else Reg (os, t.(ri)) in
      Hashtbl.add env id slot)
    fdef.locals;

  List.iter (fun (t, id) -> Hashtbl.add gl_env.typing id t) fdef.locals;
  List.iter (fun (t, id) -> Hashtbl.add gl_env.typing id t) fdef.args;

  let lc = ref (-1) in
  let label_fmt () =
    lc := !lc + 1;
    "__" ^ fdef.name ^ "_" ^ string_of_int !lc
  in
  let rec save_regs save fetch i ri regs =
    if i < ri then
      save_regs (save @@ push regs.(i)) (pop regs.(i) @@ fetch) (i + 1) ri regs
    else (save, fetch)
  in

  let rec tr_expr ri e =
    let tr_args li =
      List.fold_right (fun e code -> code @@ tr_expr ri e @@ push t.(ri)) li nop
    in
    match e with
    | Var id -> (
        match Hashtbl.find_opt env id with
        | Some (Offset os) -> lw t.(ri) os fp
        | Some (Reg (_, rg)) -> move t.(ri) rg
        | None -> (
            match Hashtbl.find_opt gl_env.typing id with
            | Some _ -> la t.(ri) id @@ lw t.(ri) 0 t.(ri)
            | None when fun_kind = Method ->
                tr_expr ri (Read (Str (Var "__self__", id)))
            | None ->
                failwith
                  (Printf.sprintf "Variable %s does not belong to the program"
                     id)))
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
        our semantic, especially when calling a function on
        a deref pointer or a global var.
      *)
        let alloc_a = req_register e1 in
        let alloc_b = req_register e2 in
        let op = tr_bop bop in
        let e1_, e2_ = if alloc_b > alloc_a then (e2, e1) else (e1, e2) in
        let eva = tr_expr ri e1_ in
        let evb = tr_expr (ri + 1) e2_ in
        eva @@ evb
        @@
        if alloc_b > alloc_a then op t.(ri) t.(ri + 1) t.(ri)
        else op t.(ri) t.(ri) t.(ri + 1)
    | Ref id -> (
        Printf.printf "ref %s\n" id;
        match Hashtbl.find_opt env id with
        | Some (Offset os) -> addi t.(ri) fp os
        | Some (Reg (os, pr)) ->
          (*
            Stores the register value on stack
            for common use and pointer arithmetic.
          *)
          Hashtbl.replace env id (Offset os);
          addi t.(ri) fp os @@
          sw pr 0 t.(ri)
          (* failwith "Bad memory access!" *)
        | None -> la t.(ri) id)
    | Deref e -> tr_expr ri e @@ lw t.(ri) 0 t.(ri)
    | Call (fn, args) when fun_kind = Method ->
        tr_expr ri (MCall (Deref (Var "__self__"), fn, args))
    | Call (id, args) ->
        (*
          Save common use registers on the stack 
          from the caller before calling given function.
        *)
        Printf.printf "Fun call %s\n" id;

        let call =
          if Hashtbl.mem gl_env.functions id then jal id
          else tr_expr ri (Var id) @@ jalr t.(ri)
        in
        let s, f = save_regs nop nop 0 ri t in
        let args_code = tr_args args in
        s @@ args_code @@ call
        @@ addi sp sp (4 * List.length args)
        @@ move t.(ri) t.(0)
        @@ f
        (*
    
        Arguments are first placed in the a0, a1, a2, a3
        registers.
        
        If more, then stay in t_i after evaluation and
        t_i is pushed on the stack.
        
        For now, since the optimization isn't relevant
        we just push arguments on the stack

      *)
        (* List.iteri
             (fun i (_, id) -> Hashtbl.add env id (Offset (4 * (i + 1))))
             fdef.args;
              let rec aux i = function
             | [] -> nop
             | h :: tl when i < 4 ->
                 aux (i + 1) tl
                 @@ tr_expr ri h
                 @@ move a.(i) t.(ri)
                 @@ subi sp sp 4
             | l ->
                 List.fold_right
                   (fun e code -> code @@ tr_expr ri e @@ push t.(ri))
                   l nop
           in
           let argc = List.length args in
           aux 0 args @@ jal id @@ addi sp sp (4 * argc) *)
    | DynCall (e, args) ->
        let s, f = save_regs nop nop 0 ri t in
        let args_asm = tr_args args in
        let func = tr_expr ri e in
        s @@ args_asm @@ func
        @@ jalr t.(ri)
        @@ addi sp sp (4 * List.length args)
        @@ move t.(ri) t.(0)
        @@ f
    | MCall ((Var id | Deref (Var id)), fname, args) -> (
        (*
      We look for the method either in the structure or
      in its parent, in case we didn't find it. This implies
      the overrinding.   
    *)
        let typ = Hashtbl.find gl_env.typing id in
        Printf.printf "%s.%s\n" (show_typ typ) fname;
        match typ with
        | TStruct sct_name | TPointer (TStruct sct_name) ->
            (*
              Evaluates the type of each argument   
            *)
            let args_type =
              List.fold_right (fun a li -> te_expr a gl_env :: li) args []
            in

            let nm, os =
              get_sct_method_offset sct_name fname args_type gl_env.structs
            in
            Printf.printf "Offset of method %s is %d from %s\n" fname os nm;
            tr_expr ri
              (DynCall
                 ( Deref (BOP (Add, Var (desc_fmt nm), Val (Int (4 * (os + 1))))),
                   Var id :: args ))
        | _ -> failwith "Not a struct")
    | MCall (_, _, _) -> failwith "Unknown object!"
    | Alloc e ->
        tr_expr ri e @@ move a0 t.(ri) @@ li v0 9 @@ syscall @@ move t.(ri) v0
    | Read m -> tr_expr ri (Deref (tr_mem m))
    | New (name, args) ->
        (*
          We add a slot for :
          - the descriptor
          - each variable of structure and its parent (included super var)   
        *)
        let size = 4 + sizeof_struct name gl_env.structs in
        Printf.printf "allocated struct %s size : %d\n" name size;

        (*
          To call the constructor and other methods
          within the expr, we assign a temporary name
          to our register.
        *)
        let var = "__var_" ^ name ^ "__" in

        Hashtbl.add env var (Reg (0, t.(ri)));
        Hashtbl.add gl_env.typing var (TStruct name);

        let stc = Hashtbl.find gl_env.structs name in
        let prt_desc =
          match stc.parent with
          | Some ps -> la t.(ri + 1) (desc_fmt ps)
          | _ -> li t.(ri + 1) 0
        in

        tr_expr ri (Alloc (Val (Int size)))
        @@ la t.(ri + 1) (desc_fmt name)
        @@ sw t.(ri + 1) 0 t.(ri)
        @@ prt_desc
        @@ sw t.(ri + 1) 4 t.(ri)
        @@ tr_expr (ri + 1) (MCall (Var var, "constructor", args))
    | NewArray (_, e) -> tr_expr ri (Alloc e)
    | _ ->
        failwith
          "Unknown expression kind, maybe the typing or analysis process got \
           wrong?"
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
    | Str (Var id, field) | Str (Deref (Var id), field) ->
        let t = Hashtbl.find gl_env.typing id in
        let def =
          match t with
          | TStruct name | TPointer (TStruct name) -> name
          | _ -> failwith "This is not a structure type!"
        in
        let offset = get_field_offset def field 1 gl_env.structs in
        Printf.printf "Offset of %s.%s is %d\n" def field offset;
        tr_mem (Arr (Var id, Val (Int offset)))
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
        tr_expr ri_offset e
        @@
        match Hashtbl.find_opt env id with
        | Some (Offset os) -> sw t0 os fp
        | Some (Reg (_, rg)) -> move rg t0
        | None when fun_kind = Method ->
            tr_stm
              (Write { cnt = (Str (Deref (Var "__self__"), id), e); l = ld.l })
        | None -> la t1 id @@ sw t0 0 t1)
    | If (ld, s1, s2) ->
        let then_label = label_fmt () in
        let end_label = label_fmt () in

        tr_expr ri_offset ld.cnt @@ bnez t0 then_label @@ tr_seq s2 @@ b end_label
        @@ label then_label @@ tr_seq s1 @@ label end_label
    | While (ld, s) ->
        let test_label = label_fmt () in
        let code_label = label_fmt () in

        b test_label @@ label code_label @@ tr_seq s @@ label test_label
        @@ tr_expr ri_offset ld.cnt @@ bnez t0 code_label
    | Return ld -> tr_expr ri_offset ld.cnt @@ subi sp fp 4 @@ pop ra @@ pop fp @@ jr ra
    | Expr ld -> tr_expr ri_offset ld.cnt
    | Write ld ->
        let d, e = ld.cnt in
        tr_expr ri_offset (tr_mem d) @@ tr_expr (ri_offset + 1) e @@ sw t1 0 t0
  in
  let method_fmt sn (m : fun_def) =
    let args_fmt =
      List.fold_right (fun (arg_t, _) str -> hash_type arg_t ^ str) m.args ""
    in
    sn ^ "_" ^ m.name ^ "_" ^ args_fmt
  in
  let tr_struct_methods sct =
    
    let desc_name = desc_fmt sct.name in
    (*
       It generates the assembly code of all of the
       methods intern to the struct and get their address.
     *)
    Hashtbl.add gl_env.typing desc_name (TPointer TInt);
    List.fold_right
      (fun (me : fun_def) code ->
        let sname = method_fmt sct.name me in
        let analysis = dataflow me.code in
        let chailin = reg_dist analysis 0 in
        Hashtbl.add gl_env.functions sname me;
        tr_function gl_env chailin
          {
            me with
            name = sname;
            args = (TPointer (TStruct sct.name), "__self__") :: me.args;
          }
          Method
        @@ code)
      sct.methods nop
  in
  let tr_struct sct =
    (*
      The structure descriptor first contains the parent
      descriptor address, then its method' address
    *)
    let desc_size = 4 * (List.length sct.methods + 1) in
    Printf.printf "%s descriptor size %d\n" sct.name desc_size;
    let desc_name = desc_fmt sct.name in
    let assign =
      Assign { cnt = (desc_name, Alloc (Val (Int desc_size))); l = -1 }
    in
    let parent = Write { cnt = (Raw (Var desc_name), Val (Int 0)); l = -1 } in
    let _, desc =
      List.fold_right
        (fun (m : fun_def) (i, code) ->
          Printf.printf "%s.%s is written in %d\n" sct.name m.name i;
          ( i - 1,
            tr_stm
              (Write
                 {
                   cnt =
                     ( Raw (BOP (Add, Var desc_name, Val (Int (i * 4)))),
                       Ref (method_fmt sct.name m) );
                   l = -1;
                 })
            @@ code ))
        sct.methods (List.length sct.methods, nop)
    in
    tr_stm assign @@ tr_stm parent @@ desc
  in
  let desc_asm =
    if fdef.name = "main" then
      Hashtbl.fold
        (fun _ v code -> tr_struct_methods v @@ code)
        gl_env.structs nop
      @@ label "main"
      @@ Hashtbl.fold (fun _ v code -> tr_struct v @@ code) gl_env.structs nop
    else label fdef.name
  in
  desc_asm @@ push fp @@ push ra @@ addi fp sp 4
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
    let struct_env = Hashtbl.create 16 in
    let globals =
      List.fold_right
        (fun (s : struct_def) gl ->
          Hashtbl.add struct_env s.name s;
          let dn = desc_fmt s.name in
          (TVoid, dn) :: gl)
        prog.structs prog.globals
    in
    let type_env = Hashtbl.create 16 in
    List.iter (fun (t, id) -> Hashtbl.add type_env id t) globals;
    List.iter
      (fun (f : fun_def) -> Hashtbl.add type_env f.name f.return)
      prog.functions;

    let function_env = Hashtbl.create 16 in
    List.iter
      (fun (f : fun_def) -> Hashtbl.add function_env f.name f)
      (__builtin_print_char_def :: __builtin_print_int_def :: prog.functions);

    let env =
      { typing = type_env; functions = function_env; structs = struct_env }
    in

    Printf.printf "[i] type-checking the imp code\n";

    let functions =
      List.fold_right
        (fun (f : fun_def) fl -> tpt_fun f env :: fl)
        prog.functions []
    in
(* 
    let opt_funcs =
      List.fold_right
        (fun f l ->
          let s = f.code in
          let analysis = dataflow s in
          let chailin = reg_dist analysis 0 in
          Hashtbl.iter (fun k v -> Printf.printf "%s -> %d\n" k v) chailin;
          let rd = deadcode_reduction s [] analysis in
          { f with code = rd } :: l)
        functions []
    in *)
    Printf.printf "Generating assembly code\n";

    let fn_asm =
      List.fold_right
        (fun (def : Ast.fun_def) code -> 
          let s = def.code in
          let analysis = dataflow s in
          let chailin = reg_dist analysis 0 in
          Hashtbl.iter (fun k v -> Printf.printf "%s -> %d\n" k v) chailin;
          let rd = deadcode_reduction s [] analysis in
          tr_function env chailin { def with code = rd } Function @@ code)
        functions nop
    in
    let text = head @@ fn_asm @@ __builtin_print_int @@ __builtin_print_char in
    let data =
      List.fold_right
        (fun (_, id) code -> label id @@ dword [ 0 ] @@ code)
        globals nop
    in
    let prog = { text; data } in
    let output_fn = Filename.chop_suffix f ".imp" ^ ".asm" in
    let out = open_out output_fn in
    let fmt = formatter_of_out_channel out in
    Mips.print_program fmt prog;
    pp_print_flush fmt ();
    close_out out
end
