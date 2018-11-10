type program =
  | AProgram of top_level list

and top_level =
  | ExpTop of expression

and expression =
  | ConstExp of int * Ploc.t
  | DiffExp of expression * expression * Ploc.t
  | IsZeroExp of expression * Ploc.t
  | IfExp of expression * expression * expression * Ploc.t
  | VarExp of int * Ploc.t
  | LetExp of expression * expression * Ploc.t
  | ProcExp of expression * Ploc.t
  | ApplyExp of expression * expression * Ploc.t
  | LetRecExp of (expression list) * expression * Ploc.t
  | MultiLetExp of (expression list) * expression * Ploc.t
  | ExceptionExp of expression * expression
  | RaiseExp of expression

               
val parse : char Stream.t -> program

