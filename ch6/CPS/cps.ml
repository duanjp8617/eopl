open Syntax

type simple_exp =
  | SimpConstExp of int
  | SimpVarExp of string
  | SimpDiffExp of simple_exp * simple_exp
  | SimpIsZeroExp of simple_exp

and 
