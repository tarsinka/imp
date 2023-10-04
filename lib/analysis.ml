open Ast

(*
  That file manages the static analysis of IMP.
  
  To describe the CFG, we factorize our
  statement type, defined in ast.ml
*)

type ss_block =
  | BAssign of string * expr
  | BWrite of mem * expr
  | BExpr of expr
  | BBool of expr
  | BReturn of expr
  | BSkip
[@@deriving show]

module VarSet = Set.Make (String)

let rec vars = function
  | Var id -> VarSet.singleton id
  | Val _ | New _ | NewArray _ -> VarSet.empty
  | BOP (_, e1, e2) -> VarSet.union (vars e1) (vars e2)
  | Deref e | Alloc e -> vars e
  | Ref v -> VarSet.singleton v
  | Call (_, args) ->
      List.fold_right (fun e set -> VarSet.union (vars e) set) args VarSet.empty
  | Read m -> vars_mem m

and vars_mem = function
  | Raw e -> vars e
  | Arr (e1, e2) -> VarSet.union (vars e1) (vars e2)
  | Str (e, _) -> vars e

let gen = function
  | BAssign (_, e) -> vars e
  | BWrite (m, e) -> VarSet.union (vars_mem m) (vars e)
  | BExpr e -> vars e
  | BBool e -> vars e
  | BReturn e -> vars e
  | BSkip -> VarSet.empty

let kill = function BAssign (id, _) -> VarSet.singleton id | _ -> VarSet.empty

type block = {
  label : ss_label;
  kind : ss_block;
  next : block list;
  vars_in : VarSet.t;
  vars_out : VarSet.t; [@equal fun a b -> a.label = b.label]
}

let zero_block =
  {
    label = 0;
    kind = BSkip;
    next = [];
    vars_in = VarSet.empty;
    vars_out = VarSet.empty;
  }

let rec block_of post stm =
  match stm with
  | Assign ld ->
      let id, e = ld.cnt in
      { zero_block with label = ld.l; kind = BAssign (id, e); next = [ post ] }
  | If (ld, s1, s2) ->
      let b1 = blocks_of ~post s1 in
      let b2 = blocks_of ~post s2 in
      { zero_block with label = ld.l; kind = BBool ld.cnt; next = [ b1; b2 ] }
  | While (ld, s) ->
      let b = blocks_of ~post s in
      { zero_block with label = ld.l; kind = BBool ld.cnt; next = [ post; b ] }
  | Return ld ->
      { zero_block with label = ld.l; kind = BReturn ld.cnt; next = [ post ] }
  | Expr ld ->
      { zero_block with label = ld.l; kind = BExpr ld.cnt; next = [ post ] }
  | Write ld ->
      let m, e = ld.cnt in
      { zero_block with label = ld.l; kind = BWrite (m, e); next = [ post ] }

and blocks_of ?(post = zero_block) = function
  | [] -> post
  | h :: t ->
      let next_block = blocks_of t in
      block_of next_block h

let rec dataflow block =
  let next = List.fold_right (fun b l -> dataflow b :: l) block.next [] in

  (*
    From bottom to the top static analysis.
    If the new next blocks analysis are different
    from the original one, then we alter the current
    block analysis.    
  *)
  let rec aux la lb =
    match la with
    | [] -> false
    | h :: t ->
        let hlb = List.hd lb in
        if VarSet.equal h.vars_in hlb.vars_in then aux t (List.tl lb) else true
  in
  let _ = aux next block.next in
  let v_out =
    List.fold_right (fun b v -> VarSet.union v b.vars_in) next VarSet.empty
  in
  let v_in =
    VarSet.union (gen block.kind) (VarSet.diff v_out (kill block.kind))
  in
  { block with vars_in = v_in; vars_out = v_out; next }

let rec print_dataflow b =
  Printf.printf "%d -> %s\n{" b.label (show_ss_block b.kind);
  VarSet.iter (Printf.printf "%s, ") b.vars_in;
  Printf.printf "}\n{";
  VarSet.iter (Printf.printf "%s, ") b.vars_out;
  Printf.printf "}\n";
  List.iter print_dataflow b.next
