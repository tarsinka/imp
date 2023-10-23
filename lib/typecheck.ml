open Ast

type env = {
  typing : (string, typ) Hashtbl.t;
  functions : (string, fun_def) Hashtbl.t;
  structs : (string, struct_def) Hashtbl.t;
}

let int_reduction = function
  | TPointer t -> TPointer t
  | TChar -> TChar
  | _ -> TInt

(*
  Type-check of an expression   
*)

let rec tp_expr e ty env =
  match e with
  | BOP (_, e1, e2) ->
      let tya = int_reduction (te_expr e1 env) in
      let tyb = int_reduction (te_expr e2 env) in
      tp_expr e1 tya env && tp_expr e2 tyb env
  | Alloc e -> tp_expr e TInt env
  | Deref e -> tp_expr e (TPointer TVoid) env
  | Call (fn, args) -> (
      Printf.printf "Check function call of %s\n" fn;
      let fundef = Hashtbl.find_opt env.functions fn in
      match fundef with
      | None -> tp_expr (DynCall (Var fn, args)) ty env
      | Some def ->
          if List.length def.args <> List.length args then
            failwith
              (Printf.sprintf "Function %s has too few/many arguments!\n" fn)
          else
            List.fold_right2
              (fun e (t, _) b -> tp_expr e t env && b)
              args def.args true)
  | _ -> te_expr e env = ty

(*
  Type information of an expression
*)

and te_expr e env =
  match e with
  | Val (Int _) -> TInt
  | Val (Char _) -> TChar
  | Val (Bool _) -> TBool
  | Val (String _) -> TArray TChar
  | Val (Array l) ->
      let ty =
        List.fold_right
          (fun e p ->
            let tye = te_expr e env in
            if p = TVoid || tye = p then tye else failwith "")
          l TVoid
      in
      TArray ty
  | Var id -> (
      let typ = Hashtbl.find_opt env.typing id in
      match typ with
      | None -> failwith (Printf.sprintf "%s is an unknown variable" id)
      | Some t -> t)
  | BOP ((Add | Sub | Mul | Div | Mod | And | Or | Xor | Ror | Rol), e1, e2)
    -> (
      let tya = te_expr e1 env in
      let tyb = te_expr e2 env in
      match (tya, tyb) with TPointer a, TPointer _ -> TPointer a | _ -> TInt)
  | BOP ((Le | Lt | Ge | Gt | Eq | Ne), _, _) -> TBool
  | Alloc _ -> TPointer TVoid
  | Ref id ->
      let t = Hashtbl.find env.typing id in
      TPointer t
  | New (id, _) -> TStruct id
  | Deref e -> (
      let t = te_expr e env in
      match t with TPointer a -> a | _ -> TVoid)
  | Call (fn, _) ->
      let fundef = Hashtbl.find env.functions fn in
      fundef.return
  | MCall (Var id, fname, _) -> (
      let typ = Hashtbl.find env.typing id in
      match typ with
      | TStruct sct_name ->
          let sct = Hashtbl.find env.structs sct_name in
          List.fold_right
            (fun (m : fun_def) t -> if m.name = fname then m.return else t)
            sct.methods TVoid
      | _ -> failwith "Unknown structure!")
  | MCall (_, _, _) -> TVoid
  | DynCall (e, _) -> te_expr (Deref e) env
  | Read m -> te_mem m env
  | NewArray (t, _) -> TArray t

(*
    Type information on a memory access   
  *)

and te_mem m env =
  match m with
  | Raw _ -> TInt
  | Arr (e1, _) -> (
      let atyp = te_expr e1 env in
      match atyp with TPointer t | TArray t -> t | _ -> TVoid)
  | Str (Var id, fd) -> (
      let sctopt = Hashtbl.find_opt env.typing id in
      match sctopt with
      | Some (TStruct sname) ->
          let sct = Hashtbl.find env.structs sname in
          let parent_fields =
            match sct.parent with
            | Some p ->
                let parent_sct = Hashtbl.find env.structs p in
                parent_sct.fields
            | None -> []
          in
          let rec help = function
            | [] -> TVoid
            | (ty, s) :: t -> if s = fd then ty else help t
          in
          help (sct.fields @ parent_fields)
      | _ -> failwith "This is not a structure!")
  | Str (Read m, _) -> te_mem m env
  | _ -> failwith "Semantic error!"

let rec tp_stm s fn env =
  match s with
  | Assign ld ->
      let id, e = ld.cnt in
      let t = Hashtbl.find env.typing id in
      tp_expr e t env
  | If (ld, s1, s2) ->
      tp_expr ld.cnt TBool env && tp_seq s1 fn env && tp_seq s2 fn env
  | While (ld, s) -> tp_expr ld.cnt TBool env && tp_seq s fn env
  | Return ld -> tp_expr ld.cnt fn.return env
  | Expr ld -> tp_expr ld.cnt TVoid env
  | Write ld ->
      let m, e = ld.cnt in
      let mtype = te_mem m env in
      tp_expr e mtype env

and tp_seq seq fn env =
  match seq with [] -> true | h :: t -> tp_stm h fn env && tp_seq t fn env

let tp_fun fn env =
  let fill (t, id) = Hashtbl.add env.typing id t in
  List.iter fill fn.locals;
  List.iter fill fn.args;
  tp_seq fn.code fn env
