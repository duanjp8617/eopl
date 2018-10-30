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
  | LetExp of string * expression * expression * Ploc.t
  | ProcExp of string * expression * Ploc.t
  | ApplyExp of expression * expression * Ploc.t
  | ProcDefExp of string * string * expression * Ploc.t
  | LetRecExp of (expression list) * expression * Ploc.t
  | LetDefExp of string * expression * Ploc.t
  | MultiLetExp of (expression list) * expression * Ploc.t
  | SetExp of string * expression * Ploc.t
  | BeginExp of (expression list) * Ploc.t
            
val parse : char Stream.t -> program

