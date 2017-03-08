(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

module Funenv:ENV =
  struct
    type 't env = string -> 't
    exception WrongBindlist
    let emptyenv(x) = function y -> x
    let applyenv(x,y) = x y
    let bind(r, l, e) = function lu -> if lu = l
                                       then e
                                       else applyenv(r, lu)
    let rec bindlist(r, il, el) = match (il, el) with
      | ([],[]) -> r
      | i::il1, e::el1 -> bindlist (bind(r, i, e), il1, el1)
      | _ -> raise WrongBindlist
  end
