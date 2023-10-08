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
      | Some stm when len > 0 ->
          stm :: deadcode_reduction t (List.hd block.next)
      | Some stm -> stm :: []
      | None when len > 0 -> deadcode_reduction t (List.hd block.next)
      | None -> [])

module Node = Set.Make (Int)
module Graph = Map.Make (Int)

let print_graph g =
  Graph.iter
    (fun i s ->
      Printf.printf "%d -> " i;
      Node.iter (fun e -> Printf.printf "%d, " e) s;
      Printf.printf "\n")
    g;
  Printf.printf "\n"

(*
    Builds a complete graph given a vertice set   
  *)
let chailin_graph v base =
  let rec aux g = function
    | [] -> g
    | h :: t ->
        let ts =
          match Graph.find_opt h g with None -> Node.empty | Some s -> s
        in
        let tr =
          Graph.add h (Node.diff (Node.union ts v) (Node.singleton h)) g
        in
        aux tr t
  in
  aux base (Node.elements v)

(*
    Returns the general degree of a node   
  *)

let deg g e =
  let fo = Graph.find_opt e g in
  match fo with None -> 0 | Some ns -> Node.cardinal ns

(*
  Returns a list of the node sorted by
  their degree.   
*)

let get_reg_by_degree g =
  let ds = Graph.fold (fun k v l -> (k, Node.cardinal v) :: l) g [] in
  let sds = List.sort (fun (_, d) (_, d') -> -1 * Stdlib.compare d d') ds in
  List.fold_right (fun (k, _) c -> k :: c) sds []

(*
    Given a register indice base,
    a list of neighbouring of a node
    and the Map of the current used registers,
    returns the next allocable register.   
  *)

let get_reg ri ns vs =
  Node.fold
    (fun n i ->
      let j = Hashtbl.find vs n in
      if j = i then i + 1 else if j > i then j + 1 else i)
    ns ri

let reg_dist block ri =
  let rec help gr = function
    | [] -> gr
    | h :: t ->
        let varset =
          VarSet.fold
            (fun s v ->
              let hash = Hashtbl.hash s in
              Node.add hash v)
            h.vars_in Node.empty
        in
        let g = chailin_graph varset gr in
        (* print_graph g; *)
        let nxt_g = help g t in
        help nxt_g h.next
  in
  let ag = help Graph.empty [ block ] in
  let sr = get_reg_by_degree ag in
  let dist = Hashtbl.create (Graph.cardinal ag) in
  Graph.iter (fun i _ -> Hashtbl.add dist i (-1)) ag;
  List.iter
    (fun n ->
      (* Printf.printf "%d\n" n; *)
      let ns = Graph.find n ag in
      (* Printf.printf "%d\n" (Node.cardinal ns); *)
      let i = get_reg ri ns dist in
      Hashtbl.replace dist n i)
    sr;
  dist
