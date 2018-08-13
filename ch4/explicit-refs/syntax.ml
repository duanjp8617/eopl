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
  | ProcExp of string * expression * Ploc.t
  | ApplyExp of string * expression * Ploc.t
  | ProcDefExp of string * string * expression * Ploc.t
  | LetRecExp of (expression list) * expression * Ploc.t
  | NewRefExp of expression * Ploc.t
  | DeRefExp of expression * Ploc.t
  | SetRefExp of expression * expression  * Ploc.t
  | BeginEndExp of (expression list) * Ploc.t
    
let g = Grammar.gcreate (Plexer.gmake ())

let p = Grammar.Entry.create g "program"
let t = Grammar.Entry.create g "top_level"
let e = Grammar.Entry.create g "expression"
let l = Grammar.Entry.create g "list"
      
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
      | "proc"; "("; var = LIDENT; ")"; exp = e  -> ProcExp (var, exp, loc)
      | "("; var = LIDENT; exp = e; ")" -> ApplyExp (var, exp, loc)
      | "let"; var = LIDENT; "="; exp1 = e; "in"; exp2 = e -> LetExp (var, exp1, exp2, loc)
      | "letrec"; ls = LIST1 l; "in"; exp2 = e -> LetRecExp (ls, exp2, loc)
      | "newref"; "("; exp = e; ")" -> NewRefExp (exp, loc)
      | "deref"; "("; exp = e; ")" -> DeRefExp (exp, loc)
      | "setref"; "("; exp1 = e; ","; exp2 = e; ")" -> SetRefExp (exp1, exp2, loc)
      | "begin"; exp_ls = LIST1 e SEP ";"; "end" -> BeginEndExp (exp_ls, loc)
      ]
];

l : [
      [var1 = LIDENT; "("; var2 = LIDENT; ")"; "="; exp1 = e -> ProcDefExp (var1, var2, exp1, loc)]
];

END
