(*
* Elaborato di Linguaggi A.A. 2016/17
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

type ide = string
type exp =
      | Eint of int
      | Ebool of bool
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
