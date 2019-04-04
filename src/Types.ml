open AST
   
type t =
  Number
| String
| Boolean
| Function of t*t
| L of t
| Bot

let rec pp_type fmt = function
    Number -> Format.fprintf fmt "num"
  | String -> Format.fprintf fmt "str"
  | Boolean -> Format.fprintf fmt "bool"
  | Function (t1, t2) -> Format.fprintf fmt "(%a -> %a)" pp_type t1 pp_type t2
  | L t -> Format.fprintf fmt "%a list" pp_type t
  | Bot -> Format.fprintf fmt "'a"

let rec type_of_syntax_type = function
    AST.Number -> Number
  | AST.String -> String
  | AST.Boolean -> Boolean
  | AST.Function (l, r) -> Function (type_of_syntax_type l, type_of_syntax_type r)
  | AST.List t -> L (type_of_syntax_type t)
  | AST.Bottom -> Bot

let type_compatible a b =
  a = b || b = Bot
  
let rec type_check env = function
  (* EF-TY-VAR *)
  | Var v -> env v

  (* EF-TY-LET *)
  | Let (x, e1, e2) ->
     let x_ty = type_check env e1 in
     let env' = bind env x x_ty in
     type_check env' e2
    
  (* EF-TY-NUM *)
  | Val (Num _) -> Number

  (* EF-TY-STR *)
  | Val (Str _) -> String

  (* EF-TY-TT *)
  | Val (Bool true) -> Boolean

  (* EF-TY-TF *)
  | Val (Bool false) -> Boolean

  | Val (List []) -> Bot

  | Val (List (a::bs)) ->
     let ta = type_check env (Val a) in
     let tb = type_check env (Val (List bs)) in
     assert (type_compatible ta tb);
     L ta

  (* EF-TY-FUN *)
  | Val (Arr (_, t, e)) -> type_of_syntax_type t

  (* EF-TY-PLUS *)
  | Plus (e1, e2) ->
     assert (type_check env e1 = Number);
     assert (type_check env e2 = Number);
     Number

  (* EF-TY-TIMES *)
  | Times (e1, e2) ->
     assert (type_check env e1 = Number);
     assert (type_check env e2 = Number);
     Number

  (* EF-TY-CAT *)
  | Cat (e1, e2) ->
     assert (type_check env e1 = String);
     assert (type_check env e2 = String);
     String

  (* EF-TY-LEN *)
  | Length e ->
     assert (type_check env e = String);
     Number

  (* EF-TY-EQ *)
  | Equal (e1, e2) ->
     assert (type_check env e1 = Number);
     assert (type_check env e2 = Number);
     Boolean

  (* EF-TY-ITE *)
  | Ite (e, e1, e2) ->
     assert (type_check env e = Boolean);
     let e1_t = type_check env e1 in
     let e2_t = type_check env e2 in
     assert (e1_t = e2_t);
     e1_t

  (* EF-TY-LAM *)
  | Lam (x, t1, e) ->
     let t1 = type_of_syntax_type t1 in
     let env' = bind env x t1 in
     let t2 = type_check env' e in
     Function (t1, t2)

  (* EF-TY-AP *)
  | Ap (e1, e2) ->
     begin
       match (type_check env e1) with
         Function (t1, t2) ->
         let t_e2 = type_check env e2 in
         assert (t1 = t_e2);
         t2
       | _ -> failwith "Cannot apply argument to non-function!"
     end

  | Empty -> Bot

  | Cons (e1, e2) ->
     let t1 = type_check env e1 in
     let t2 = type_check env e2 in
     assert (type_compatible (L t1) t2);
     L t1

  | Case (e0, x, y, e1, Empty) ->
     let t0 = type_check env e0 in
     let env' = bind env x Bot in
     let env'' = bind env' y (L Bot) in
     let t1 = type_check env'' e1 in
     assert(t0 = t1);
     t0

  | Case (e0, x, y, e1, Cons (a, b)) ->
     let t0 = type_check env e0 in
     let env' = bind env x (type_check env a) in
     let env'' = bind env' y (type_check env b) in
     let t1 = type_check env'' e1 in
     assert(t0 = t1);
     t0

  | Case (_, _, _, _, _) ->
     assert false
     
    
let empty_env x = failwith (Format.sprintf "Could not find variable '%s' in type environment" x)
