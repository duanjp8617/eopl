open Syntax
open Cps_out

val cps_transformation : expression -> cps_exp

exception CpsError of string

val eval_program : program -> unit
