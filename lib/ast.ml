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
  | Alloc of expr
  | Ref of string
  | Deref of expr
  | Call of string * expr list
  | DynCall of expr * expr list
  | Read of mem
  | New of string
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
}

let get_field_offset sct field =
  let rec aux i = function
    | [] -> -1
    | (_, id) :: t -> if id = field then i else aux (i + 1) t
  in
  aux 0 sct.fields

let sizeof_struct sct =
  (*
    Here we consider that struct and array are
    allocated on the heap. So we assume there is
    only pointer.
  *)
  List.fold_right (fun _ size -> size + 4) sct.fields (-4)

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
