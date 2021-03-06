(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

type ide = string
type exp =
      | Eint of int
      | Ebool of bool
      | Estring of string
      | Den of ide
      | Prod of exp * exp
      | Sum of exp * exp
      | Diff of exp * exp
      | Eq of exp * exp
      | Minus of exp
      | Iszero of exp
      | Or of exp * exp
      | And of exp * exp
      | Not of exp
      | Ifthenelse of exp * exp * exp
      | Let of ide * exp * exp
      | Fun of ide list * exp
      | Appl of exp * exp list
      | Rec of ide * exp
      | Len of exp
      | Conc of exp * exp
      | Subs of exp * exp * exp
