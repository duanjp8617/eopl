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
  | ProcDefExp of string * string * expression * Ploc.t
  | LetRecExp of (expression list) * expression * Ploc.t
  | LetDefExp of string * expression * Ploc.t
  | MultiLetExp of (expression list) * expression * Ploc.t
    
let g = Grammar.gcreate (Plexer.gmake ())

let p = Grammar.Entry.create g "program"
let t = Grammar.Entry.create g "top_level"
let e = Grammar.Entry.create g "expression"
let l = Grammar.Entry.create g "list"
let letdef = Grammar.Entry.create g "letdef"
      
let parse = Grammar.Entry.parse p

let rec apply_env str env =
  match env with
  | [] -> -1
  | 

EXTEND
p : [
      [tops = LIST0 t -> AProgram tops]
];

t : [
      [exp = e; ";" -> ExpTop exp]
];

e : [
      [ num = INT -> (fun env -> ConstExp (int_of_string num, loc))
      | "-"; "("; exp1 = e; ","; exp2 = e; ")" -> (fun env -> DiffExp (exp1 env, exp2 env, loc))
      | "is_zero"; "("; exp = e; ")" -> (fun env -> IsZeroExp(exp env, loc))
      | "if"; exp1 = e; "then"; exp2 = e; "else"; exp3 = e ->
         (fun env -> IfExp (exp1 env, exp2 env, exp3 env, loc))
      | var = LIDENT -> VarExp (var, loc)
      | "proc"; "("; var = LIDENT; ")"; exp = e  -> ProcExp (var, exp, loc)
      | "("; exp1 = e; exp2 = e; ")" -> ApplyExp (exp1, exp2, loc)
      | "let"; var = LIDENT; "="; exp1 = e; "in"; exp2 = e -> LetExp (var, exp1, exp2, loc)
      | "letrec"; ls = LIST1 l; "in"; exp2 = e -> LetRecExp (ls, exp2, loc)
      | "mlet"; ls = LIST1 letdef; "in"; exp = e -> MultiLetExp (ls, exp, loc)
      ]
];

l : [
      [var1 = LIDENT; "("; var2 = LIDENT; ")"; "="; exp1 = e -> ProcDefExp (var1, var2, exp1, loc)]
];

letdef : [
      [var = LIDENT; "="; exp=e -> LetDefExp (var, exp, loc)]
];

END

