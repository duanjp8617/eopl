type program =
  | AProgram of top_level list

and top_level =
  | ExpTop of expression

and expression =
  | ConstExp of int 
  | DiffExp of expression * expression
  | IsZeroExp of expression
  | IfExp of expression * expression * expression 
  | VarExp of string 
  | LetExp of string * expression * expression 
  | ProcExp of (string list) * expression
  | ApplyExp of expression * (expression list) 
  | ProcDefExp of string * (string list) * expression 
  | LetRecExp of (expression list) * expression
    
let g = Grammar.gcreate (Plexer.gmake ())

let p = Grammar.Entry.create g "program"
let t = Grammar.Entry.create g "top_level"
let e = Grammar.Entry.create g "expression"
let l = Grammar.Entry.create g "letrec_list"
      
let parse = Grammar.Entry.parse p


EXTEND
p : [
      [tops = LIST0 t -> AProgram tops]
];

t : [
      [exp = e; ";" -> ExpTop exp]
];

e : [
      [ num = INT -> ConstExp (int_of_string num)
      | "-"; "("; exp1 = e; ","; exp2 = e; ")" -> DiffExp (exp1, exp2)
      | "is_zero"; "("; exp = e; ")" -> IsZeroExp(exp)
      | "if"; exp1 = e; "then"; exp2 = e; "else"; exp3 = e -> IfExp (exp1, exp2, exp3)
      | var = LIDENT -> VarExp (var)
      | "proc"; "("; var_ls = LIST1 LIDENT SEP ","; ")"; exp = e  -> ProcExp (var_ls, exp)
      | "("; exp1 = e; exp_ls = LIST1 e; ")" -> ApplyExp (exp1, exp_ls)
      | "let"; var = LIDENT; "="; exp1 = e; "in"; exp2 = e -> LetExp (var, exp1, exp2)
      | "letrec"; ls = LIST1 l; "in"; exp2 = e -> LetRecExp (ls, exp2)
      ]
];

l : [
      [var1 = LIDENT; "("; var_ls = LIST1 LIDENT SEP ","; ")"; "="; exp1 = e -> ProcDefExp (var1, var_ls, exp1)]
];

END

