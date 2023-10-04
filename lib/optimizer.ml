open Ast
open Analysis

(*
  Dead code elimination optimization   
*)

let rec deadcode_reduction_stm stm block =
  match stm with
  | Assign ld ->
      let id, _ = ld.cnt in
      if VarSet.exists (fun var -> var = id) block.vars_out then
        Some (Assign ld)
      else None
  | If (ld, s1, s2) ->
      let ba = List.hd block.next in
      let bb = List.nth block.next 1 in
      let ra = deadcode_reduction s1 ba in
      let rb = deadcode_reduction s2 bb in
      Some (If (ld, ra, rb))
  | While (ld, s) ->
      let rs = deadcode_reduction s (List.nth block.next 1) in
      Some (While (ld, rs))
  | Return ld -> Some (Return ld)
  | Expr ld -> Some (Expr ld)
  | Write ld -> Some (Write ld)

and deadcode_reduction seq block =
  match seq with
  | [] -> seq
  | h :: t -> (
      let res = deadcode_reduction_stm h block in
      let len = List.length block.next in
      match res with
      | Some stm when len > 0 -> stm :: deadcode_reduction t (List.hd block.next)
      | Some stm -> stm :: []
      | None when len > 0 -> deadcode_reduction t (List.hd block.next)
      | None -> [])

(*
  Constant operations reduction    
*)

(* let rec constant_reduction_expr e =
  match e with
  | Val t -> ( match t with Int i -> Some i | Bool b -> Some b)
  | BOP (bop, e1, e2) -> (
      let op = bop_to_arithmetic bop in
      let a = constant_reduction_expr e1 in
      let b = constant_reduction_expr e2 in
      match a with
      | Some i -> ( match b with Some j -> Some (op i j) | None -> None)
      | None -> None)
  | _ -> None *)
