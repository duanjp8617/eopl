type simple_exp =
  | SimpConstExp of int
  | SimpVarExp of string
  | SimpProcExp of (string list) * cps_exp
  | SimpDiffExp of simple_exp * simple_exp
  | SimpIsZeroExp of simple_exp

and cps_exp =
  | CpsSimpExp of simple_exp
  | CpsIfExp of simple_exp * cps_exp * cps_exp
  | CpsLetExp of string * simple_exp * cps_exp
  | CpsApplyExp of simple_exp * (simple_exp list)
  | CpsLetRecExp of ((string * (string list) * cps_exp) list) * cps_exp
