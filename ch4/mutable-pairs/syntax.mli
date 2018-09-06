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
  | ApplyExp of string * expression * Ploc.t
  | ProcDefExp of string * string * expression * Ploc.t
  | LetRecExp of (expression list) * expression * Ploc.t
  | BeginEndExp of (expression list) * Ploc.t
  | SetExp of string * expression * Ploc.t
  | SetDynamicExp of string * expression * expression * Ploc.t
  | NewPairExp of expression * expression * Ploc.t
  | LeftExp of expression * Ploc.t
  | RightExp of expression * Ploc.t
  | SetLeftExp of expression * expression * Ploc.t
  | SetRightExp of expression * expression  * Ploc.t
                 
val parse : char Stream.t -> program

