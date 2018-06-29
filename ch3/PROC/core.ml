open Syntax

type environment = (string * expval) list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ListVal of (expval list)

let rec string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ListVal l ->
     match l with
     | [] -> "[]"
     | x :: tl ->        
        "[" ^ (List.fold_left (fun str x -> str ^ "," ^ (string_of_expval x)) (string_of_expval x)  tl) ^ "]" 

let empty_env () = []

let extend_env variable value env = (variable, value) :: env

exception MissInEnv of string

let rec apply_env variable env =
  match env with
  | (var, value) :: tl -> if variable = var then value else apply_env variable tl
  | [] -> raise (MissInEnv variable)

exception InterpreterError of string * Ploc.t

let rec eval_exp exp env =
  match exp with
  | ConstExp (num, loc) -> NumVal num
  | DiffExp (exp1, exp2, loc) ->
     let exp_val1 = eval_exp exp1 env in
     let exp_val2 = eval_exp exp2 env in
     (match (exp_val1, exp_val2) with
      | (NumVal num1, NumVal num2) -> NumVal (num1-num2)
      | _ -> raise (InterpreterError ("DiffExp has invalid type", loc)))
  | IsZeroExp (exp, loc) ->
     let exp_val = eval_exp exp env in
     (match exp_val with
      | NumVal num -> if num = 0 then BoolVal true else BoolVal false
      | _ -> raise (InterpreterError ("IsZeroExp has boolean argument", loc)))
  | IfExp (exp1, exp2, exp3, loc) ->
     let exp_val1 = eval_exp exp1 env in
     (match exp_val1 with
      | BoolVal b -> if b then eval_exp exp2 env else eval_exp exp3 env
      | _ -> raise (InterpreterError ("Condition of IfExp is not boolean", loc)))
  | VarExp (str, loc) ->
     (try apply_env str env
      with MissInEnv err_msg -> raise (InterpreterError ("Can not find variable " ^ err_msg ^ " in environment", loc)))
  | BindExp (str, exp, loc) ->
     raise (InterpreterError ("Bind expression can only appear in let", loc))
  | LetExp (ls, exp, loc) ->
     (let new_env = List.fold_right
                      (fun exp cur_env ->
                        (match exp with
                         | BindExp (v, e, loc) -> extend_env v (eval_exp e env) cur_env
                         | _ -> raise (InterpreterError ("Only bind expression can appear in let", loc))))
                      ls env
      in
     eval_exp exp new_env)
  | MinusExp (exp, loc) ->
     (let exp_val = eval_exp exp env in
      (match exp_val with
       | NumVal n -> NumVal (-1*n)
       | _ -> raise (InterpreterError ("Minus boolean variable", loc))))
  | EmptyExp loc ->
     ListVal []
  | ConsExp (head_exp, tl_list, loc) ->
     let tl_list_val = eval_exp tl_list env in
     let head_exp_val = eval_exp head_exp env in
     (match tl_list_val with      
      | ListVal l -> ListVal (head_exp_val :: l)     
      | _ -> raise (InterpreterError ("Tail is not a list", loc)))
  | CarExp (exp, loc) ->
     let exp_val = eval_exp exp env in
     (match exp_val with
      | ListVal (x :: tl) -> x
      | _ -> raise (InterpreterError ("Input to car should be a list", loc)))
  | CdrExp (exp, loc) ->
     let exp_val = eval_exp exp env in
     (match exp_val with
      | ListVal (x :: tl) -> ListVal tl
      | _ -> raise (InterpreterError ("Input to cdr should be a list", loc)))
  | NullExp (exp, loc) ->
     let exp_val = eval_exp exp env in
     (match exp_val with
      | ListVal [] -> BoolVal true
      | ListVal (x :: tl) -> BoolVal false
      | _ -> raise (InterpreterError ("Input to is_null should be a list", loc)))
  | PrintExp (exp, loc) ->
     print_endline (string_of_expval (eval_exp exp env));
     NumVal 1
  | UnpackExp (ls, exp1, exp2, loc) ->
     let exp1_val = eval_exp exp1 env in
     (match exp1_val with
      | ListVal l ->
         if List.length l <> List.length ls
         then raise (InterpreterError ("Invalid unpack expression", loc))
         else
           let new_env =  List.fold_left
                            (fun cur_env (vari, valu) ->
                              extend_env vari valu cur_env )
                            env (List.combine ls l)
           in
           eval_exp exp2 new_env
      | _ -> raise (InterpreterError ("Invalid unpack expression", loc)))
     
let eval_top_level (ExpTop e) =
  eval_exp e (empty_env ()) |> string_of_expval |> print_endline

let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list
