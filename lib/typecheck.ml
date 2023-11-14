open Ast

type env = {
  typing : (string, typ) Hashtbl.t;
  functions : (string, fun_def) Hashtbl.t;
  structs : (string, struct_def) Hashtbl.t;
}

let rec field_exists sct fd env =
  let fopt = List.find_opt (fun (_, f) -> fd = f) sct.fields in
  match fopt with
  | Some (t, _) -> t
  | None -> (
      match sct.parent with
      | Some ps ->
          let prt = Hashtbl.find env.structs ps in
          field_exists prt fd env
      | None -> TVoid)

let int_reduction = function
  | TPointer t -> TPointer t
  | TChar -> TChar
  | _ -> TInt

(*
  Type information for a given expression
*)

let rec te_expr e env =
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
            if p = TVoid || tye = p then tye
            else failwith "Array not well typed")
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
  | InstanceOf (strc, _) -> (
      let v = te_expr strc env in
      match v with
      | TStruct _ -> TBool
      | _ ->
          failwith
            (Printf.sprintf "%s is not defined as an object" (show_expr strc)))
  | Alloc _ -> TPointer TVoid
  | Ref id ->
      let t = Hashtbl.find env.typing id in
      TPointer t
  | New (id, _) ->
      (*
        Checks whether the struct is abstract
        or not.   
      *)
      let sct = Hashtbl.find env.structs id in
      if not sct.is_abstract then TStruct id
      else
        failwith (Printf.sprintf "The structure %s is defined as abstract" id)
  | Deref e -> (
      let t = te_expr e env in
      match t with TPointer a -> a | _ -> TVoid)
  | Call (fn, args) -> (
      (* Printf.printf "Check function call of %s\n" fn; *)
      let fundef = Hashtbl.find_opt env.functions fn in
      match fundef with
      | None -> te_expr (DynCall (Var fn, args)) env
      | Some def ->
          if List.length def.args <> List.length args then
            failwith
              (Printf.sprintf "Function %s has too few/many arguments" fn)
          else
            List.iter2
              (fun e (t, _) -> if te_expr e env <> t then failwith "" else ())
              args def.args;
          def.return)
  | MCall (Var id, fname, _) -> (
      let typ = Hashtbl.find env.typing id in
      match typ with
      | TStruct sct_name ->
          let sct = Hashtbl.find env.structs sct_name in
          let methods =
            match sct.parent with
            | Some p ->
                let prt = Hashtbl.find env.structs p in
                prt.methods @ sct.methods
            | _ -> sct.methods
          in
          List.fold_right
            (fun (m : fun_def) t -> if m.name = fname then m.return else t)
            methods TVoid
      | _ ->
          failwith (Printf.sprintf "%s has not been declared as a structure" id)
      )
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
          let ty = field_exists sct fd env in
          if ty <> TVoid then ty
          else
            failwith
              (Printf.sprintf "Field %s does not belong to class %s" fd sname)
      | _ -> failwith "This is not of a known structure type")
  | Str (Read m, _) -> te_mem m env
  | _ -> failwith "Semantic error"

(*
  tpt_expr returns a simplified typed AST
  when necessary.
  
  It is useful when casting expression occurs
  or when testing whether a structure is an
  instance of a particular object.

  e.g.
  InstanceOf (A, B) -> Val(Bool(True|False))
  
*)

let tpt_expr e env =
  match e with
  | InstanceOf (e, s) -> (
      let t = te_expr e env in
      match t with
      | TStruct sn ->
          let sct = Hashtbl.find env.structs sn in
          Val
            (Bool
               (s = sn
               || match sct.parent with Some ps -> s = ps | None -> false))
      | _ ->
          failwith
            (Printf.sprintf "%s is not defined as an object" (show_expr e)))
  | _ -> e

let rec inheritance_hierarchy chd trg env =
  let b = chd.name <> trg.name in
  match chd.parent with
  | Some ps when b && ps = trg.name -> true
  | Some ps when b && ps <> trg.name ->
      let prt = Hashtbl.find env.structs ps in
      inheritance_hierarchy prt trg env
  | Some _ | None -> chd.name = trg.name

(*
  tpt_stm checks whether a statement is well typed
  or not and eventually returns a type-indicationless
  AST, that is more concise.
*)

let rec tpt_stm fn s env =
  match s with
  | Assign ld -> (
      let id, e = ld.cnt in
      let e' = tpt_expr e env in
      let t_var = Hashtbl.find env.typing id in
      let t_exp = te_expr e' env in
      match (t_var, t_exp) with
      | TStruct sn, TStruct sm ->
          (*
            If the types are classes, we make
            sure the cast (if there is one) is
            correct.   
          *)
          let chd = Hashtbl.find env.structs sm in
          let trg = Hashtbl.find env.structs sn in
          if inheritance_hierarchy chd trg env then
            Assign { ld with cnt = (id, e') }
          else
            failwith
              (Printf.sprintf "Class %s is not a child class of %s" sn sm)
      | _ when t_var = t_exp -> Assign { ld with cnt = (id, e') }
      | _ ->
          failwith
            (Printf.sprintf "The assignation %s isn't well typed" (show_stm s)))
  | If (ld, s1, s2) ->
      let guard = tpt_expr ld.cnt env in
      let t_guard = te_expr guard env in
      if t_guard = TBool then
        let s1' = tpt_seq fn s1 env in
        let s2' = tpt_seq fn s2 env in
        If ({ ld with cnt = guard }, s1', s2')
      else failwith ""
  | While (ld, s) ->
      let guard = tpt_expr ld.cnt env in
      if te_expr guard env = TBool then
        let s' = tpt_seq fn s env in
        While ({ ld with cnt = guard }, s')
      else failwith ""
  | Return ld ->
      let e' = tpt_expr ld.cnt env in
      if te_expr e' env = fn.return then Return { ld with cnt = e' }
      else failwith ""
  | Expr ld ->
      let e' = tpt_expr ld.cnt env in
      let _ = te_expr e' env in
      Expr { ld with cnt = e' }
  | Write ld ->
      let m, e = ld.cnt in
      let t_m = te_mem m env in
      let e' = tpt_expr e env in
      if te_expr e' env = t_m then Write { ld with cnt = (m, e') }
      else failwith ""

and tpt_seq fn seq env =
  match seq with [] -> [] | h :: t -> tpt_stm fn h env :: tpt_seq fn t env

and tpt_fun fn env =
  let fill (t, id) = Hashtbl.add env.typing id t in
  List.iter fill fn.locals;
  List.iter fill fn.args;
  let sq = tpt_seq fn fn.code env in
  { fn with code = sq }
