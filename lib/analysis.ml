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
  | InstanceOf (e, _) -> vars e
  | Deref e | Alloc e -> vars e
  | Ref v -> VarSet.singleton v
  | Call (fn, args) -> VarSet.add fn (vars (DynCall(Var fn, args))) 
  | DynCall (_, args) | MCall (_, _, args) ->
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

module LabelSet = Set.Make (Int)

let label_of = function
  | Assign ld -> ld.l
  | If (ld, _, _) -> ld.l
  | While (ld, _) -> ld.l
  | Return ld | Expr ld -> ld.l
  | Write ld -> ld.l

let init stm = LabelSet.singleton (label_of stm)

let rec final seq =
  let aux = function
    | Assign ld -> LabelSet.singleton ld.l
    | If (_, s1, s2) -> LabelSet.union (final s1) (final s2)
    | While (ld, _) -> LabelSet.singleton ld.l
    | Return ld | Expr ld -> LabelSet.singleton ld.l
    | Write ld -> LabelSet.singleton ld.l
  in
  match seq with
  | [] -> LabelSet.empty
  | h :: t -> LabelSet.union (aux h) (final t)

type block = {
  label : ss_label;
  kind : ss_block;
  vars_in : VarSet.t;
  vars_out : VarSet.t; [@equal fun a b -> a.label = b.label]
}

module Edge = struct
  type t = int * int

  let compare a b = Stdlib.compare a b
end

module EdgeSet = Set.Make (Edge)
module BlockMap = Map.Make (Int)

type analysis = { flow : EdgeSet.t; blocks : block BlockMap.t }

let succ n es = EdgeSet.fold (fun (a, b) l -> if a = n then b :: l else l) es []
let pred n es = EdgeSet.fold (fun (a, b) l -> if b = n then a :: l else l) es []

let zero_block =
  { label = 0; kind = BSkip; vars_in = VarSet.empty; vars_out = VarSet.empty }

let rec block_of bm = function
  | Assign ld ->
      let id, e = ld.cnt in
      BlockMap.add ld.l
        { zero_block with label = ld.l; kind = BAssign (id, e) }
        bm
  | If (ld, s1, s2) ->
      let be =
        BlockMap.add ld.l
          { zero_block with label = ld.l; kind = BBool ld.cnt }
          bm
      in
      let bs = blocks_of be s1 in
      blocks_of bs s2
  | While (ld, s) ->
      let be =
        BlockMap.add ld.l
          { zero_block with label = ld.l; kind = BBool ld.cnt }
          bm
      in
      blocks_of be s
  | Return ld ->
      BlockMap.add ld.l
        { zero_block with label = ld.l; kind = BReturn ld.cnt }
        bm
  | Expr ld ->
      BlockMap.add ld.l { zero_block with label = ld.l; kind = BExpr ld.cnt } bm
  | Write ld ->
      let m, e = ld.cnt in
      BlockMap.add ld.l
        { zero_block with label = ld.l; kind = BWrite (m, e) }
        bm

and blocks_of bm = function
  | [] -> bm
  | h :: t ->
      let b = block_of bm h in
      blocks_of b t

let rec flow_of pred es stm =
  let label = label_of stm in
  let flow =
    match stm with
    | Assign _ | Return _ | Expr _ | Write _ -> EdgeSet.empty
    | If (_, s1, s2) ->
        let fa = flows_of label es s1 in
        let fb = flows_of label es s2 in
        EdgeSet.union fa fb
    | While (_, s) ->
        let final_lb = final s in
        let back =
          LabelSet.fold
            (fun fl es -> EdgeSet.union es (EdgeSet.singleton (fl, label)))
            final_lb EdgeSet.empty
        in
        EdgeSet.union (flows_of label es s) back
  in
  EdgeSet.union flow (EdgeSet.add (pred, label) es)

and flows_of pred es = function
  | [] -> es
  | h :: t ->
      let f = if pred <> -1 then flow_of pred es h else es in
      flows_of (label_of h) f t

let dataflow seq =
  let flow = flows_of (-1) EdgeSet.empty seq in
  let blocks = blocks_of BlockMap.empty seq in
  let analysis = { flow; blocks } in

  Printf.printf "CFG done\n";

  let rec iterate a bl altered =
    match bl with
    | [] -> (altered, a)
    | m :: t ->
        let block_m = BlockMap.find m a.blocks in
        let in_ =
          VarSet.union (gen block_m.kind)
            (VarSet.diff block_m.vars_out (kill block_m.kind))
        in
        let succs = succ m a.flow in
        let out_ =
          List.fold_right
            (fun s vs ->
              let bs = BlockMap.find s a.blocks in
              VarSet.union vs bs.vars_in)
            succs VarSet.empty
        in
        let alt =
          (not (VarSet.equal in_ block_m.vars_in))
          || not (VarSet.equal out_ block_m.vars_out)
        in
        iterate
          {
            a with
            blocks =
              BlockMap.add m
                { block_m with vars_in = in_; vars_out = out_ }
                a.blocks;
          }
          t (altered || alt)
  in
  let rec fixpoint a =
    let alt, it =
      iterate a (BlockMap.fold (fun i _ l -> i :: l) a.blocks []) false
    in
    if alt then fixpoint it else it
  in
  fixpoint analysis

let print_vars v =
  Printf.printf "{";
  VarSet.iter (fun s -> Printf.printf "%s," s) v;
  Printf.printf "}\n"

let print_analysis a =
  BlockMap.iter
    (fun i b ->
      Printf.printf "%d:\n" i;
      print_vars b.vars_in;
      print_vars b.vars_out)
    a.blocks
