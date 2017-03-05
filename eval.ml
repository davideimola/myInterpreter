(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)
exception Nonstorable
exception Nonexpressible
type eval = | Int of int
            | Bool of bool
            | Novalue
type dval = | Dint of int
            | Dbool of bool
            | Unbound
            | Dloc of loc
type mval = | Mint of int
            | Mbool of bool
            | Undefined
let evaltomval e = match e with
            | Int n -> Mint n
            | Bool n -> Mbool n
            | _ -> raise Nonstorable
let mvaltoeval m = match m with
            | Mint n -> Int n
            | Mbool n -> Bool n
            | _ -> Novalue
let evaltodval e = match e with
            | Int n -> Dint n
            | Bool n -> Dbool n
            | Novalue -> Unbound
let dvaltoeval e = match e with
            | Dint n -> Int n
            | Dbool n -> Bool n
            | Dloc n -> raise Nonexpressible
            | Unbound -> Novalue
