type program =
  | AProgram of top_level list

and top_level =
  | ExpTop of expression

and expression =
  | ConstExp of int * Ploc.t
  | DiffExp of expression * expression * Ploc.t
  | IsZeroExp of expression * Ploc.t
  | IfExp of expression * expression * expression * Ploc.t
  | VarExp of string * Ploc.t
  | LetExp of string * expression * expression * Ploc.t
  | ProcExp of (string list) * expression * Ploc.t
  | RecProcDef of string * (string list) * expression * Ploc.t
  | ApplyExp of expression * (expression list) * Ploc.t
    
let g = Grammar.gcreate (Plexer.gmake ())

let p = Grammar.Entry.create g "program"
let t = Grammar.Entry.create g "top_level"
let e = Grammar.Entry.create g "expression"
let r = Grammar.Entry.create g "rec_proc"
      
let parse = Grammar.Entry.parse p


EXTEND
p : [
      [tops = LIST0 t -> AProgram tops]
];

t : [
      [exp = e; ";" -> ExpTop exp]
];

e : [
      [ num = INT -> ConstExp (int_of_string num, loc)
      | "-"; "("; exp1 = e; ","; exp2 = e; ")" -> DiffExp (exp1, exp2, loc)
      | "is_zero"; "("; exp = e; ")" -> IsZeroExp(exp, loc)
      | "if"; exp1 = e; "then"; exp2 = e; "else"; exp3 = e -> IfExp (exp1, exp2, exp3, loc)
      | var = LIDENT -> VarExp (var, loc)
      | "proc"; "("; ls = LIST1 LIDENT SEP ","; ")"; exp = e  -> ProcExp (ls, exp, loc)
      | "("; exp1 = e; exp_ls = LIST1 e; ")" -> ApplyExp (exp1, exp_ls, loc)
      | "let"; var = LIDENT; "="; exp1 = e; "in"; exp2 = e -> LetExp (var, exp1, exp2, loc)
      ]
];

r : [
      [ var1 = LIDENT; "("; ls = LIST1 LIDENT SEP ","; ")"; "="; exp1 = e -> RecProcDef (var1, ls, exp1, loc)
      ]
];

END
