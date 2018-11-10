type program =
  | AProgram of top_level list

and top_level =
  | ExpTop of expression

and expression =
  | ConstExp of int * Ploc.t
  | DiffExp of expression * expression * Ploc.t
  | IsZeroExp of expression * Ploc.t
  | IfExp of expression * expression * expression * Ploc.t
  | VarExp of int * Ploc.t
  | LetExp of expression * expression * Ploc.t
  | ProcExp of expression * Ploc.t
  | ApplyExp of expression * expression * Ploc.t
  | LetRecExp of (expression list) * expression * Ploc.t
  | MultiLetExp of (expression list) * expression * Ploc.t
  | ExceptionExp of expression * expression
  | RaiseExp of expression
                  
type parse_helper =
  | ProcDefExp of string * string * ((string list) -> expression) * Ploc.t
  | LetDefExp of string * ((string list) -> expression) * Ploc.t
    
let g = Grammar.gcreate (Plexer.gmake ())

let p = Grammar.Entry.create g "program"
let t = Grammar.Entry.create g "top_level"
let e = Grammar.Entry.create g "expression"
let l = Grammar.Entry.create g "list"
let letdef = Grammar.Entry.create g "letdef"
      
let parse = Grammar.Entry.parse p
          
let apply_env str env =
  let rec go str env pos =
    (match env with
     | [] -> -1
     | hd :: tl -> if str = hd then pos else go str tl (pos+1))
  in
  (go str env 0)
 

    
EXTEND
p : [
      [tops = LIST0 t -> AProgram tops]
];

t : [
      [exp = e; ";" -> ExpTop (exp [])]
];

e : [
      [ num = INT -> (fun env -> ConstExp (int_of_string num, loc))
      | "-"; "("; exp1 = e; ","; exp2 = e; ")" -> (fun env -> DiffExp (exp1 env, exp2 env, loc))
      | "is_zero"; "("; exp = e; ")" -> (fun env -> IsZeroExp(exp env, loc))
      | "if"; exp1 = e; "then"; exp2 = e; "else"; exp3 = e ->
         (fun env -> IfExp (exp1 env, exp2 env, exp3 env, loc))
      | var = LIDENT -> (fun env -> VarExp (apply_env var env, loc))
      | "proc"; "("; var = LIDENT; ")"; exp = e  -> (fun env -> ProcExp (exp (var :: env), loc))
      | "("; exp1 = e; exp2 = e; ")" -> (fun env -> ApplyExp (exp1 env, exp2 env, loc))
      | "let"; var = LIDENT; "="; exp1 = e; "in"; exp2 = e ->
         (fun env -> LetExp (exp1 env, (exp2 (var :: env)), loc))
      | "letrec"; ls = LIST1 l; "in"; body = e ->
         (fun env ->
           let fname_ls = List.map (fun (ProcDefExp (fname,_,_,_)) -> fname) ls in
           let exp_ls = List.map (fun (ProcDefExp (_, arg, pbody, _)) -> pbody (arg :: (fname_ls @ env))) ls in
            LetRecExp (exp_ls, body (fname_ls @ env), loc))
      | "mlet"; ls = LIST1 letdef; "in"; body = e ->
         (fun env ->
            let var_ls = List.map (fun (LetDefExp(var, _, _)) -> var) ls in
            let exp_ls = List.map (fun (LetDefExp(_, exp, _)) -> (exp env)) ls in
            MultiLetExp (exp_ls, body (var_ls @ env), loc))
      ]
];

l : [
      [var1 = LIDENT; "("; var2 = LIDENT; ")"; "="; exp1 = e -> ProcDefExp (var1, var2, (fun env -> exp1 env), loc)]
];

letdef : [
      [var = LIDENT; "="; exp=e -> LetDefExp (var, (fun env -> exp env), loc)]
];

END

