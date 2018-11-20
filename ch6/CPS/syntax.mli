type program =
  | AProgram of top_level list

and top_level =
  | ExpTop of expression

and expression =
  | ConstExp of int 
  | DiffExp of expression * expression
  | IsZeroExp of expression
  | IfExp of expression * expression * expression 
  | VarExp of string 
  | LetExp of string * expression * expression 
  | ProcExp of (string list) * expression
  | ApplyExp of expression * (expression list) 
  | ProcDefExp of string * (string list) * expression 
  | LetRecExp of (expression list) * expression
                 
val parse : char Stream.t -> program
