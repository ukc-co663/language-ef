open AST
   
type types =
  Number
| String
| Boolean

let rec type_check env = function
  (* E-TY-VAR *)
  | Var v -> env v

  (* E-TY-LET *)
  | Let (x, e1, e2) ->
     let x_ty = type_check env e1 in
     let env' = bind env x x_ty in
     type_check env' e2
    
  (* E-TY-NUM *)
  | Val (Num _) -> Number

  (* E-TY-STR *)
  | Val (Str _) -> String

  (* E-TY-TT *)
  | Val (Bool true) -> Boolean

  (* E-TY-TT *)
  | Val (Bool false) -> Boolean

  (* E-TY-PLUS *)
  | Plus (e1, e2) ->
     assert (type_check env e1 = Number);
     assert (type_check env e2 = Number);
     Number

  (* E-TY-TIMES *)
  | Times (e1, e2) ->
     assert (type_check env e1 = Number);
     assert (type_check env e2 = Number);
     Number

  (* E-TY-CAT *)
  | Cat (e1, e2) ->
     assert (type_check env e1 = String);
     assert (type_check env e2 = String);
     String

  (* E-TY-LEN *)
  | Length e ->
     assert (type_check env e = String);
     Number

  (* E-TY-EQ *)
  | Equal (e1, e2) ->
     assert (type_check env e1 = Number);
     assert (type_check env e2 = Number);
     Boolean

  (* E-TY-ITE *)
  | Ite (e, e1, e2) ->
     assert (type_check env e = Boolean);
     let e1_t = type_check env e1 in
     let e2_t = type_check env e2 in
     assert (e1_t = e2_t);
     e1_t
 
