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
  | NewRefExp of expression * Ploc.t
  | DeRefExp of expression * Ploc.t
  | SetRefExp of expression * expression  * Ploc.t
  | BeginEndExp of (expression list) * Ploc.t
  | SetExp of string * expression * Ploc.t
  | SetDynamicExp of string * expression * expression * Ploc.t
  | NotExp of expression * Ploc.t
    
and statement =
  | AssignStmt of string * expression * Ploc.t
  | PrintStmt of expression * Ploc.t
  | AListOfStmt of (statement list) * Ploc.t
  | IfStmt of expression * statement * statement * Ploc.t
  | WhileStmt of expression * statement * Ploc.t
  | DefinitionStmt of (string list) * statement * Ploc.t
                   
(* val parse : char Stream.t -> program *)
val parse : char Stream.t -> statement                                 

