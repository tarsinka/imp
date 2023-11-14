type typ =
  | TInt
  | TChar
  | TBool
  | TString
  | TPointer of typ
  | TArray of typ
  | TStruct of string
  | TVoid
[@@deriving show]

let rec hash_type = function
  | TInt -> "i"
  | TChar -> "c"
  | TBool -> "b"
  | TString -> "s"
  | TPointer t -> "p" ^ hash_type t
  | TArray t -> "a" ^ hash_type t
  | TStruct str -> "x_" ^ str ^ "_"
  | TVoid -> "v"

type var = typ * string

type bop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | And
  | Or
  | Xor
  | Rol
  | Ror
  | Lt
  | Le
  | Gt
  | Ge
  | Eq
  | Ne
[@@deriving show]

type ss_label = int [@@deriving show]
type 'a labelled = { cnt : 'a; l : ss_label } [@@deriving show]

type expr =
  | Val of val_type
  | Var of string
  | BOP of bop * expr * expr
  | InstanceOf of expr * string
  | Alloc of expr
  | Ref of string
  | Deref of expr
  | Call of string * expr list
  | MCall of expr * string * expr list
  | DynCall of expr * expr list
  | Read of mem
  | New of string * expr list
  | NewArray of typ * expr

and mem = Raw of expr | Arr of expr * expr | Str of expr * string
[@@deriving show]

and val_type =
  | Int of int
  | Char of char
  | Bool of bool
  | String of string
  | Array of expr list
[@@deriving show]

type stm =
  | Assign of (string * expr) labelled
  | If of expr labelled * seq * seq
  | While of expr labelled * seq
  | Return of expr labelled
  | Expr of expr labelled
  | Write of (mem * expr) labelled
[@@deriving show]

and seq = stm list [@@deriving show]

type fun_def = {
  name : string;
  args : (typ * string) list;
  locals : (typ * string) list;
  code : seq;
  return : typ;
}

type struct_def = {
  name : string;
  fields : (typ * string) list;
  methods : fun_def list;
  parent : string option;
  is_abstract : bool;
}

let rec get_field_offset sct_name field offset env =
  let sct = Hashtbl.find env sct_name in
  let pfs, os =
    match sct.parent with
    | Some ps ->
        let prt = Hashtbl.find env ps in
        (List.length prt.fields, get_field_offset ps field offset env)
    | None -> (0, -1)
  in
  let rec aux i = function
    | [] -> -1
    | (_, id) :: t -> if id = field then i else aux (i + 1) t
  in
  if os = -1 then aux (pfs + offset) sct.fields else os

let rec get_sct_method_offset sct_name fname args_type env =
  let args_type_cmp a =
    if List.length a = List.length args_type then
      List.fold_right2 (fun (x, _) y b -> x = y && b) a args_type true
    else false
  in
  let sct = Hashtbl.find env sct_name in
  let rec aux i = function
    | [] -> (
        match sct.parent with
        | Some ps -> get_sct_method_offset ps fname args_type env
        | None -> (sct_name, -1))
    | (m : fun_def) :: t ->
        if m.name = fname && args_type_cmp m.args then (sct_name, i)
        else aux (i + 1) t
  in
  aux 0 sct.methods

let rec sizeof_struct sct_name env =
  (*
    Here we consider that struct and array are
    allocated on the heap. So we assume there is
    only pointer.
  *)
  let sct = Hashtbl.find env sct_name in
  let parent_size =
    match sct.parent with
    | Some p ->
        Printf.printf "%s : %s\n" sct_name p;
        sizeof_struct p env
    | None -> 0
  in
  List.fold_right (fun _ size -> size + 4) sct.fields parent_size

type program = {
  globals : (typ * string) list;
  functions : fun_def list;
  structs : struct_def list;
}

let str_to_array str =
  let len = String.length str in
  let rec aux i =
    if i >= len - 1 then Val (Int 0) :: [] else Val (Char str.[i]) :: aux (i + 1)
  in
  aux 1
