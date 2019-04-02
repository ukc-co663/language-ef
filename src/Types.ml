open AST
   
type t =
  Number
| String
| Boolean
| Function of t*t

let rec pp_type fmt = function
    Number -> Format.fprintf fmt "num"
  | String -> Format.fprintf fmt "str"
  | Boolean -> Format.fprintf fmt "bool"
  | Function (t1, t2) ->
     Format.fprintf fmt "(%a -> %a)" pp_type t1 pp_type t2

let rec type_of_syntax_type = function
    AST.Number -> Number
  | AST.String -> String
  | AST.Boolean -> Boolean
  | AST.Function (l, r) -> Function (type_of_syntax_type l, type_of_syntax_type r)

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

  | Val (Arr (_, t, e)) -> type_of_syntax_type t

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

  | Lam (x, t1, e) ->
     let t1 = type_of_syntax_type t1 in
     let env' = bind env x t1 in
     let t2 = type_check env' e in
     Function (t1, t2)

  | Ap (e1, e2) ->
     begin
       match (type_check env e1) with
         Function (t1, t2) ->
         let t_e2 = type_check env e2 in
         assert (t1 = t_e2);
         t2
       | _ -> failwith "Cannot apply argument to non-function!"
     end

    
let empty_env x = failwith (Format.sprintf "Could not find variable '%s' in type environment" x)


(* let f be \ {num -> num} (x) . (x+3) in (f) (3)  *)
