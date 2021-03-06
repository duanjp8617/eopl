open Syntax
open Cps_out

let var_id = ref 0

let gen_var () =
  var_id := (!var_id + 1);
  "var_" ^ (string_of_int !var_id)

(*Note, simple cps_in expression is constant, variable and procedure.
  When doing cps transformation, the transformation ends when all operands
  of a complex expression becomes simple enough*)
  
let is_exp_simple exp =
  match exp with
  | ConstExp _ -> true
  | VarExp _ -> true
  | ProcExp _ -> true
  | _ -> false

exception CpsError of string
            
let rec cps_of_exp_list exp_l all_simple_builder =
  let rec helper l pred examined =
    (match l with
     | [] -> (List.rev examined, [])
     | hd :: tl ->
        (if pred hd
         then helper tl pred (hd :: examined)
         else (List.rev examined, l))) in
  let (fst, snd) = helper exp_l (fun exp -> is_exp_simple exp) [] in
  (match snd with
   | [] -> all_simple_builder (List.map cps_of_simple_exp fst)
   | hd :: tl ->
      let var_name = gen_var () in
      cps_of_exp hd
        (SimpProcExp ([var_name],
                      cps_of_exp_list (fst @ (VarExp var_name :: tl)) all_simple_builder)))
  
and cps_of_simple_exp exp =
  match exp with
  | ConstExp n -> SimpConstExp n
  | VarExp var -> SimpVarExp var
  | ProcExp (str_l, exp) ->
     let var_name = gen_var () in
     SimpProcExp (str_l @ [var_name], cps_of_exp exp (SimpVarExp var_name))
  | _ -> raise (CpsError "exp is not simple")

and cps_of_exp exp k_exp = 
  match exp with
  | ConstExp n -> CpsApplyExp (k_exp, [SimpConstExp n])
  | VarExp var -> CpsApplyExp (k_exp, [SimpVarExp var])
  | ProcExp (str_l, exp) ->
     let var_name = gen_var () in 
     CpsApplyExp (k_exp, [SimpProcExp (str_l @ [var_name], cps_of_exp exp (SimpVarExp var_name))])
  | DiffExp (exp1, exp2) ->
     cps_of_exp_list (exp1 :: [exp2])
       (fun l -> CpsApplyExp (k_exp, [SimpDiffExp ((List.nth l 0), (List.nth l 1))]))
  | IsZeroExp (exp) ->
     cps_of_exp_list [exp] (fun l -> CpsApplyExp (k_exp, [SimpIsZeroExp (List.hd l)]))
  | IfExp (exp1, exp2, exp3) ->
     cps_of_exp_list [exp1]
       (fun l -> CpsIfExp ((List.hd l),(cps_of_exp exp2 k_exp),(cps_of_exp exp3 k_exp)))
  | LetExp (var, exp1, exp2) ->
     cps_of_exp_list [exp1]
       (fun l -> CpsLetExp (var, (List.hd l), (cps_of_exp exp2 k_exp)))
  | ApplyExp (exp1, exp_ls) ->
     cps_of_exp_list (exp1 :: exp_ls)
       (fun l -> CpsApplyExp ((List.hd l), (List.tl l) @ [k_exp]))
  | ProcDefExp _ -> raise (CpsError "Impossible")
  | LetRecExp (exp_l, exp_body) ->
     let simples = List.map
                     (fun exp ->
                       match exp with
                       | ProcDefExp (pname, var_ls, exp) ->
                          let var_name = gen_var () in
                          (pname, var_ls @ [var_name], cps_of_exp exp (SimpVarExp var_name))
                       | _ -> raise (CpsError "Impossible"))                     
                     exp_l in
     CpsLetRecExp (simples, (cps_of_exp exp_body k_exp))
       
       
let cps_transformation exp =
  cps_of_exp exp (SimpProcExp (["fuck"], CpsSimpExp (SimpVarExp "fuck")))

let eval_program (AProgram l) =
  List.iter (fun (ExpTop exp) -> cps_transformation exp |> eval_cps_exp) l
