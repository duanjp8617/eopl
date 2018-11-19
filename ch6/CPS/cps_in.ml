open Syntax
open Cps_out

let var_id = ref 0

let gen_var () =
  var_id := (!var_id + 1);
  "var_" ^ (string_of_int !var_id)

let is_exp_simple exp =
  match exp with
  | ConstExp _ -> true
  | VarExp _ -> true
  | ProcExp _ -> true
  | DiffExp (exp1, exp2) ->
     (match (exp1, exp2) with
      | (ConstExp _, ConstExp _) -> true
      | (ConstExp _, VarExp _) -> true
      | (VarExp _, ConstExp _) -> true
      | _ -> false)
  | IsZeroExp (exp1) ->
     (match exp1 with
      | ConstExp _ -> true
      | VarExp _ -> true
      | _ -> false)
  | _ -> false          
       
let rec cps_of_exp exp k_exp =
  match exp with
  | ConstExp n -> CpsApplyExp (k_exp, [SimpConstExp n])
  | VarExp var -> CpsApplyExp (k_exp, [SimpVarExp var])
  | ProcExp (str_l, exp) ->
     let var_name = gen_var () in 
     CpsApplyExp (k_exp, [SimpProcExp (str_l @ [var_name], cps_of_exp exp (SimpVarExp var_name))])
  | DiffExp (exp1, exp2) ->
     
and cps_of_exp_list exp_l all_simple_builder =
  let rec helper remaining pred examined =
    (match l with
     | [] -> (List.rev examined, [])
     | hd :: tl ->
        (if pred hd
         then helper tl pred (hd :: examined)
         else (List.rev examined, l))) in
  let (fst, snd) = helper exp_l (fun exp -> is_exp_simple exp) [] in
  let var_name = gen_var () in
  cps_of_exp (List.head snd) (SimpProcExp ([var_name], ) )
  
