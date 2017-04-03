(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

(* TYPE EVAL: EXPRESSIBLE VALUES *)
type eval =
        | Int of int
        | Bool of bool
        | String of string
        | Funval of efun
        | Novalue
and efun = (dval list) * (mval store) -> eval

(* TYPE DVAL: DENOTABLE VALUES *)
and dval =
        | Dint of int
        | Dbool of bool
        | DString of string
        | Unbound
        | Dloc of loc
        | Dfunval of efun
        | Dprocval of proc
and proc = (dval list) * (mval store) -> mval store

(* TYPE MVAL: STORABLE VALUES *)
and mval =
        | Mint of int
        | Mbool of bool
        | MString of string
        | Undefined



(* TYPE CONVERSIONS *)
exception Nonstorable
exception Nonexpressible

let evaltomval e =
      (match e with
      | Int n      -> Mint n
      | Bool n     -> Mbool n
      | _          -> raise Nonstorable)

let mvaltoeval m =
      (match m with
      | Mint n     -> Int n
      | Mbool n    -> Bool n
      | _          -> Novalue)

let evaltodval e =
      (match e with
      | Int n      -> Dint n
      | Bool n     -> Dbool n
      | Novalue    -> Unbound
      | Funval n   -> Dfunval n
      | _          -> failwith("Not a valid type eval"))

let dvaltoeval e =
      (match e with
      | Dint n     -> Int n
      | Dbool n    -> Bool n
      | Dloc n     -> raise Nonexpressible
      | Dfunval n  -> Funval n
      | Dprocval n -> raise Nonexpressible
      | Unbound    -> Novalue
      | DString n  -> String n)
