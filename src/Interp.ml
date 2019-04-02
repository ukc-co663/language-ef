open AST

let rec interp env = function
    Var v -> env v
  
  | Val (Num n) -> Num n

  | Val (Str s) -> Str s

  | Val (Bool b) -> Bool b

  | Val (Arr (v, t, e)) -> Arr (v, t, e)

  (* EF-SE-PLVAL *)
  | Plus (Val (Num a), Val (Num b)) -> Num (a + b)

  (* EF-SE-PLR *)
  | Plus (Val (Num a), e2) ->
     let e2' = interp env e2 in
     interp env (Plus (Val (Num a), Val e2'))
                                          
  (* EF-SE-PLL *)
  | Plus (e1, e2) ->
     let e1' = interp env e1 in
     interp env (Plus (Val e1', e2))

  (* EF-SE-TMVAL *)
  | Times (Val (Num a), Val (Num b)) -> Num (a * b)

  (* EF-SE-TMR *)
  | Times (Val (Num a), e2) ->
     let e2' = interp env e2 in
     interp env (Times (Val (Num a), Val e2'))
                                          
  (* EF-SE-TML *)
  | Times (e1, e2) ->
     let e1' = interp env e1 in
     interp env (Plus (Val e1', e2))

  (* EF-SE-CAT *)
  | Cat (Val (Str e1), Val (Str e2)) -> Str (e1 ^ e2)
     
  (* EF-SE-CATR *)
  | Cat (Val e1, e2) ->
     let e2' = interp env e2 in
     interp env (Cat (Val e1, Val e2'))

  (* EF-SE-CATL *)
  | Cat (e1, e2) ->
     let e1' = interp env e1 in
     interp env (Cat (Val e1', e2))

  (* EF-SE-LEN *)
  | Length (Val (Str s)) -> Num (String.length s)

  (* EF-SE-LENR *)
  | Length e1 ->
     let e1' = interp env e1 in
     interp env (Length (Val e1'))

  (* EF-SE-EQT *)
  | Equal (Val (Num a), Val (Num b)) when a = b -> Bool true

  (* EF-SE-EQF *)
  | Equal (Val (Num a), Val (Num b)) when a <> b -> Bool false

  (* EF-SE-EQR *)
  | Equal (Val (Num a), e2) ->
     let e2' = interp env e2 in
     interp env (Equal (Val (Num a), Val e2'))

  (* EF-SE-EQL *)
  | Equal (e1, e2) ->
     let e1' = interp env e1 in
     interp env (Equal (Val e1', e2))

  (* EF-SE-ITET *)
  | Ite (Val (Bool true), e1, _) ->
     interp env e1

  (* EF-SE-ITEF *)
  | Ite (Val (Bool false), _, e2) ->
     interp env e2

  (* EF-SE-ITE *)
  | Ite (e, e1, e2) ->
     let e' = interp env e in
     interp env (Ite (Val e', e1, e2))

  (* EF-SE-LETV *)
  | Let (x, (Val v), e2) ->
     let env' = bind env x v in
     interp env' e2

  (* EF-SE-LETM *)
  | Let (x, e1, e2) ->
     let e1' = interp env e1 in
     interp env (Let (x, Val e1', e2))

  (* EF-SE-LAM*)
  | Lam (x, t, e) -> Arr (x, t, e)

  (* EF-SE-APL *)
  | Ap (Val (Arr (x, _, e)) , Val e2) ->
     let env' = bind env x e2 in
     interp env' e

  (* EF-SE-APR *)
  | Ap (Val e1, e2) ->
     let e2' = interp env e2 in
     interp env (Ap (Val e1, Val e2'))
     
  (* EF-SE-AP *)
  | Ap (e1, e2) ->
     let e1' = interp env e1 in
     interp env (Ap (Val e1', e2))

let empty_env x = failwith (Format.sprintf "Could not find variable '%s' in evaluation environment" x)
