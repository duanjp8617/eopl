type program =
  | AProgram of top_level list

and top_level =
  | ExpTop of expression

and expression =
  | ConstExp of int * Ploc.t
  | DiffExp of expression * expression * Ploc.t
  | IsZeroExp of expression * Ploc.t
  | IfExp of expression * expression * expression * Ploc.t
  | VarExp of string * Ploc.t
  | BindExp of string * expression * Ploc.t
  | LetExp of (expression list) * expression * Ploc.t
  | MinusExp of expression * Ploc.t
  | EmptyExp of Ploc.t
  | ConsExp of expression * expression * Ploc.t
  | CarExp of expression * Ploc.t
  | CdrExp of expression * Ploc.t
  | NullExp of expression * Ploc.t
  | PrintExp of expression * Ploc.t
  | UnpackExp of (string list) * expression * expression * Ploc.t
             
val parse : char Stream.t -> program
(* val string_of_program : program -> string *)
