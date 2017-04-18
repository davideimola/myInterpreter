(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

(* IDENTIFIER *)
type ide = string

(* TYPE EXPRESSABLE *)
type exp =
      (* CONSTANTS *)
      | Eint of int
      | Ebool of bool
      | Estring of string
      | Den of ide

      (* INT OPERATIONS *)
      | Prod of exp * exp
      | Sum of exp * exp
      | Diff of exp * exp
      | Minus of exp

      (* CONTROLS *)
      | Eq of exp * exp
      | Iszero of exp
      | Or of exp * exp
      | And of exp * exp
      | Not of exp

      | Ifthenelse of exp * exp * exp
      | Val of exp
      | Let of ide * exp * exp
      | Newloc of exp
      | Fun of ide list * exp
      | Appl of exp * exp list
      | Rec of ide * exp
      | Proc of ide list * block

      (* STRING OPERATIONS *)
      | Len of exp
      | Conc of exp * exp
      | Streq of exp * exp
      | Charat of exp * exp
      | Subs of exp * exp * exp

      | Undefinedstack

(* CONSTRUCT BLOCK need for Proc and Block *)
and block = (ide * exp) list * (ide * exp) list  * com list

(* TYPE COMMAND *)
and com =
      | Assign of exp * exp                        (*  *)
      | Cifthenelse of exp * com list * com list   (*  *)
      | While of exp * com list                    (*  *)
      | Block of block                             (*  *)
      | Call of exp * exp list                     (*  *)
      | Reflect of exp                             (*  *)
