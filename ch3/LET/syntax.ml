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
  | MinusExp of expression * Ploc.t

let rec string_of_expression exp =
  match exp with
  | ConstExp (num, loc) ->
     "(Const " ^ string_of_int num ^ ")"
  | DiffExp (exp1, exp2, loc) ->
     "(Diff " ^ (string_of_expression exp1) ^ " " ^ (string_of_expression exp2) ^ ")"
  | IsZeroExp (exp, loc) ->
     "(IsZero " ^ string_of_expression exp ^ ")"
  | IfExp (cond_exp, then_exp, else_exp, loc) ->
     "(If " ^ string_of_expression cond_exp ^ " " ^ string_of_expression then_exp ^ " " ^ string_of_expression else_exp ^ ")"
  | VarExp (name, loc) ->
     "(Var " ^ name ^ ")"
  | LetExp (name, exp1, exp2, loc) ->
     "(Let (Var " ^ name ^ ") " ^ string_of_expression exp1 ^ " " ^ string_of_expression exp2 ^ ")"
  | MinusExp (exp, loc) ->
     "(Minus " ^ string_of_expression exp ^ ")"

let rec string_of_tl_list tl_list =
  match tl_list with
  | ((ExpTop e) :: tl) ->
     string_of_expression e ^ "\n" ^ string_of_tl_list tl
  | [] -> ""

let string_of_program program =
  match program with
  | AProgram tl_list -> string_of_tl_list tl_list

let g = Grammar.gcreate (Plexer.gmake ())

let p = Grammar.Entry.create g "program"
let t = Grammar.Entry.create g "top_level"
let e = Grammar.Entry.create g "expression"

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
      | "let"; var = LIDENT; "="; exp1 = e; "in"; exp2 = e -> LetExp (var, exp1, exp2, loc)
      | "minus"; "("; exp =e ; ")" -> MinusExp (exp, loc)
      ]
];

END
