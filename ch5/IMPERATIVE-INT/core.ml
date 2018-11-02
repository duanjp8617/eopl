open Syntax

type environment = expval list

and expval =
  | NumVal of int
  | BoolVal of bool
  | ProcVal of expression * (environment ref)

type continuation =
  | EndCont
  | DiffFstCont of expression * environment * continuation
  | DiffSndCont of expval * continuation 
  | IsZeroCont of continuation 
  | IfCont of expression * expression * environment * continuation 
  | LetCont of expression * environment * continuation 
  | ApplyFstCont of expression * environment * continuation
  | ApplySndCont of expval * continuation 
  | MultiLetFstCont of (expval list) * environment * (expression list) * expression * continuation
                  
let string_of_expval value =
  match value with
  | NumVal n -> string_of_int n
  | BoolVal b -> string_of_bool b
  | ProcVal _ -> "proc"               
               
let empty_env () = []

let extend_env value env = value :: env
                                  
exception MissInEnv of Ploc.t
                  
let apply_env pos env =
  let rec go pos env =
    match env with
    | hd :: tl -> if pos = 0 then Some hd else go (pos-1) tl
    | [] -> None
  in
  (if pos = -1 then None else go pos env)
    
exception InterpreterError of string * Ploc.t
exception ApplyContError of string                          


let list_empty ls =
  match ls with
  | [] -> true
  | _ -> false

let gexp = ref (ConstExp 2)
let genv = ref []
let gcont = ref EndCont
let gexp_val = ref (NumVal 1)
       
let rec eval_exp () =
  match !gexp with
  | ConstExp (num) ->
     gexp_val := (NumVal num);
     apply_cont ()
  | VarExp (pos, loc) ->
     (match (apply_env pos !genv) with
      | None -> raise (MissInEnv loc)
      | Some exp_val ->
         gexp_val := exp_val;
         apply_cont ())
  | ProcExp (body, loc) ->
     gexp_val := (ProcVal (body, (ref !genv)));
     apply_cont ()
  | DiffExp (exp1, exp2, loc) ->
     gexp := exp1;
     gcont := DiffFstCont (exp2, !genv, !gcont);
     eval_exp ()
  | IsZeroExp (exp, loc) ->
     gexp := exp;
     gcont := (IsZeroCont !gcont);
     eval_exp () 
  | IfExp(exp1, exp2, exp3, loc) ->
     gexp := exp1;
     gcont := IfCont (exp2, exp3, !genv, !gcont);
     eval_exp ()
  | LetExp (exp1, exp2, loc) ->
     gexp := exp1;
     gcont := LetCont (exp2, !genv, !gcont);
     eval_exp ()
  | ApplyExp(exp1, exp2, loc) ->
     gexp := exp1;
     gcont := ApplyFstCont (exp2, !genv, !gcont);
     eval_exp ()
  | LetRecExp(ls, body, loc) ->
     (let env_ref = ref [] in
      let proc_list = List.map (fun exp -> ProcVal (exp, env_ref)) ls in
      let new_env = List.append proc_list !genv in
      env_ref := new_env;
      genv := new_env;
      gexp := body;
      eval_exp ())
  | MultiLetExp (ls, body, loc) ->
     (match ls with
      | exp :: tl ->
         gexp := exp;
         gcont := MultiLetFstCont ([], !genv, tl, body, !gcont);
         eval_exp ()
      | _ -> raise (InterpreterError ("MultiLetExp expects non-empty expression list", loc)))
     
                          
and apply_cont () =
  match !gcont with
  | EndCont -> !gexp_val
  | DiffFstCont (exp2, env, cont) ->
     gexp := exp2;
     genv := env;
     gcont := DiffSndCont (!gexp_val, cont);
     eval_exp ()
  | DiffSndCont (exp_val1, cont) ->
     (match (exp_val1, !gexp_val) with
      | (NumVal num1, NumVal num2) ->
         gcont := cont;
         gexp_val := (NumVal (num1 - num2));
         apply_cont();                      
      | _ -> raise (ApplyContError ("DiffExp expects two integers")))
  | IsZeroCont cont ->
     (match !gexp_val with
      | NumVal num ->
         if num=0
         then
           (gcont := cont;
            gexp_val := (BoolVal true);
            apply_cont ())
         else
           (gcont := cont;
            gexp_val := (BoolVal false);
            apply_cont())
      | _ -> raise (ApplyContError ("IsZeroExp expects an integer")))
  | IfCont (exp1, exp2, env, cont) ->
     (match !gexp_val with
      | BoolVal b ->
         if b
         then
           (gexp := exp1;
            genv := env;
            gcont := cont;
            eval_exp ())
         else
           (gexp := exp2;
            genv := env;
            gcont := cont;
            eval_exp ())
      | _ -> raise (ApplyContError ("IfExp expects a boolean")))
  | LetCont (body, env, cont) ->
     gexp := body;
     genv := (!gexp_val :: env);
     gcont := cont;
     eval_exp ()
  | ApplyFstCont (exp, env, cont) ->
     gexp := exp;
     genv := env;
     gcont := (ApplySndCont (!gexp_val, cont));
     eval_exp()
  | ApplySndCont (proc_val, cont) ->
     (match proc_val with
      | ProcVal (body, p_env) ->
         gexp := body;
         genv := (!gexp_val :: !p_env);
         gcont := cont;
         eval_exp ()
      | _ -> raise (ApplyContError ("ApplyExp expects a procedure"))) 
  | MultiLetFstCont (exp_val_ls, env, tl, body, cont) ->
     (match tl with
      | exp :: let_def_tl ->
         gexp := exp;
         genv := env;
         gcont := MultiLetFstCont (exp_val_ls @ [!gexp_val], env, let_def_tl, body, cont);
         eval_exp ()
      | _ ->
         gexp := body;
         genv := (exp_val_ls @ [!gexp_val]) @ env;
         gcont := cont;
         eval_exp ())

let eval_top_level (ExpTop e) =
  (gexp := e; genv := (empty_env ()); gcont := EndCont; eval_exp())
  |> string_of_expval |> print_endline
                                                     
let value_of_program (AProgram tl_list) = List.iter eval_top_level tl_list 
