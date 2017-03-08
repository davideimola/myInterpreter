(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

module SemStack: STACK =
  struct
    type 'a stack = Empty of int | Push of 'a stack * 'a
    exception Emptystack
    exception Fullstack
    let emptystack (n, x) = Empty(n)
    let rec max = function
        | Empty n -> n
        | Push (p, a) -> max p
    let rec length = function
        | Empty n -> 0
        | Push(p, a) -> 1 + length(p)
    let push (a, p) = if length(p) = max(p)
                      then raise Fullstack
                      else Push(p, a)
    let pop = function
        | Push(p, a) -> p
        | Empty n -> raise Emptystack
    let top = function
        | Push(p, a) -> a
        | Empty n -> raise Emptystack
    let empty = function
        | Push(p, a) -> false
        | Empty n -> true
  end
