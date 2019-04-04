type variable = string

type syntax_type = 
  Number
| String
| Boolean
| Function of syntax_type * syntax_type
| List of syntax_type
| Bottom

type value =
  Num of int
| Str of string
| Bool of bool
| Arr of variable * syntax_type * expression
| List of value list

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
| Cons of expression * expression
| Empty
| Case of expression * variable * variable * expression * expression
       
let bind env k v =
  fun k' -> if k = k' then v else env k'

let rec pp_syntax_type fmt = function
    Number -> Format.fprintf fmt "num"
  | String -> Format.fprintf fmt "str"
  | Boolean -> Format.fprintf fmt "bool"
  | Function (t1, t2) ->
     Format.fprintf fmt "(%a -> %a)" pp_syntax_type t1 pp_syntax_type t2
  | List t -> Format.fprintf fmt "%a list" pp_syntax_type t
  | Bottom -> Format.fprintf fmt "'a"

let rec show_val = function
    Num n -> string_of_int n
  | Str s -> "\"" ^ s ^ "\""
  | Bool true -> "true"
  | Bool false -> "false"
  | Arr _ -> "(function)"
  | List vs -> "[" ^ (String.concat ", " (List.map show_val vs)) ^ "]"
           
let rec pp_val fmt = function
    Num n -> Format.fprintf fmt "%d" n
  | Str s -> Format.fprintf fmt "\"%s\"" s
  | Bool b -> Format.fprintf fmt "%b" b
  | Arr (_, t, _) -> Format.fprintf fmt "(function)"
  | List vs -> Format.fprintf fmt "%s" (show_val (List vs))
