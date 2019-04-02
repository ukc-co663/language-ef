type variable = string

type value =
  Num of int
| Str of string
| Bool of bool
              
type expression =
  Var of variable
| Val of value
| Plus of expression * expression
| Times of expression * expression
| Cat of expression * expression
| Length of expression
| Equal of expression * expression
| Ite of expression * expression * expression
| Let of variable * expression * expression

       
let bind env k v =
  fun k' -> if k = k' then v else env k'
