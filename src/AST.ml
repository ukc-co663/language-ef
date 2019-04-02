type variable = string

type syntax_type = 
  Number
| String
| Boolean
| Function of syntax_type * syntax_type

type value =
  Num of int
| Str of string
| Bool of bool
| Arr of variable * syntax_type * expression

and expression =
  Var of variable
| Val of value
| Plus of expression * expression
| Times of expression * expression
| Cat of expression * expression
| Length of expression
| Equal of expression * expression
| Ite of expression * expression * expression
| Let of variable * expression * expression
| Lam of variable * syntax_type * expression
| Ap of expression * expression
       
let bind env k v =
  fun k' -> if k = k' then v else env k'

let pp_val fmt = function
    Num n -> Format.fprintf fmt "%d" n
  | Str s -> Format.fprintf fmt "\"%s\"" s
  | Bool b -> Format.fprintf fmt "%b" b
  | Arr _ -> Format.fprintf fmt "(function)"
