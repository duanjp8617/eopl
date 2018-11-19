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

type environment = (string * exp_val) list 
                  
and exp_val =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of (string list) * (environment ref) * cps_exp                 

exception MissInEnv of string             
             
let extend_env var value env =
  (var, value) :: env

let rec apply_env var env =
  match env with
  | [] -> raise (MissInEnv var)
  | (var_name, var_value) :: tl ->
     if var = var_name then var_value else apply_env var tl

exception InvalidSimpExp of string

let value_of_simple_exp simple env =
  match simple with
  | SimpConstExp n -> NumVal n
  | SimpVarExp var -> apply_env var env
  | SimpProcExp (str_l, exp) -> ProcVal (str_l, (ref env), exp)
  | SimpDiffExp (exp1, exp2) ->
     (match (exp1, exp2) with
      | (SimpConstExp n1, SimpConstExp n2) -> NumVal (n1 - n2)
      | (SimpConstExp n1, SimpVarExp var) ->
         (match apply_env var env with
          | NumVal n2 -> NumVal (n1 - n2)
          | _ -> raise (InvalidSimpExp "SimpDiffExp"))
      | (SimpVarExp var, SimpConstExp n2) ->
         (match apply_env var env with
          | NumVal n1 -> NumVal (n1 - n2)
          | _ -> raise (InvalidSimpExp "SimpDiffExp"))
      | _ -> raise (InvalidSimpExp "SimpDiffExp"))
  | SimpIsZeroExp exp ->
     let helper n = if n = 0 then BoolVal true else BoolVal false in 
     (match exp with
      | SimpConstExp n -> helper n 
      | SimpVarExp var ->
         (match apply_env var env with
          | NumVal n -> helper n
          | _ -> raise (InvalidSimpExp "SimpIsZeroExp"))
      | _ -> raise (InvalidSimpExp "SimpIsZeroExp"))

exception InvalidCpsExp of string     
     
let rec value_of_cps_exp exp env =
  match exp with
  | CpsSimpExp simple -> value_of_simple_exp simple env
  | CpsIfExp (simple, exp1, exp2) ->
     (match value_of_simple_exp simple env with
      | BoolVal b ->
         (if b = true
          then value_of_cps_exp exp1 env
          else value_of_cps_exp exp2 env)
      | _ -> raise (InvalidCpsExp "CpsIfExp"))
  | CpsLetExp (var, simple, exp) ->
     value_of_cps_exp exp (extend_env var (value_of_simple_exp simple env) env)
  | CpsApplyExp (body, param_l) ->
     (match value_of_simple_exp body env with
      | ProcVal (str_l, proc_env, proc_body) ->
         let val_l = List.map (fun simple -> value_of_simple_exp simple env) param_l in
         let var_val_l = List.combine str_l val_l in 
         let final_env = List.fold_left (fun env pair -> pair :: env) !proc_env var_val_l in
         value_of_cps_exp proc_body final_env
      | _ -> raise (InvalidCpsExp "CpsApplyExp"))
  | CpsLetRecExp (proc_def_l, body) ->
     let proc_name_l = List.map (fun (name, _, _) -> name) proc_def_l in
     let empty_env = ref [] in
     let proc_l = List.map (fun (_, str_l, body) -> ProcVal (str_l, empty_env, body)) proc_def_l in
     let proc_env = List.fold_left (fun env pair -> pair :: env) env (List.combine proc_name_l proc_l) in
     empty_env := proc_env;
     value_of_cps_exp body proc_env
                   
     
       
      
     
