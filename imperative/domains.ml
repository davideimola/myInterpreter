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
        | Dstring of string
        | Unbound
        | Dloc of loc
        | Dfunval of efun
        | Dprocval of proc
and proc = (dval list) * (mval store) -> mval store

(* TYPE MVAL: STORABLE VALUES *)
and mval =
        | Mint of int
        | Mbool of bool
        | Mstring of string
        | Undefined



(* TYPE CONVERSIONS *)
exception Nonstorable
exception Nonexpressible

(* CONVERT TYPE EVAL TO TYPE MVAL *)
let evaltomval e =
      (match e with
      | Int n      -> Mint n
      | Bool n     -> Mbool n
      | String n   -> Mstring n
      | _          -> raise Nonstorable)

(* CONVERT TYPE MVAL TO TYPE EVAL *)
let mvaltoeval m =
      (match m with
      | Mint n     -> Int n
      | Mbool n    -> Bool n
      | Mstring n  -> String n
      | _          -> Novalue)

(* CONVERT TYPE EVAL TO TYPE DVAL *)
let evaltodval e =
      (match e with
      | Int n      -> Dint n
      | Bool n     -> Dbool n
      | String n   -> Dstring n
      | Novalue    -> Unbound
      | Funval n   -> Dfunval n
      | _          -> failwith("Not a valid type eval"))

(* CONVERT TYPE DVAL TO TYPE EVAL *)
let dvaltoeval e =
      (match e with
      | Dint n     -> Int n
      | Dbool n    -> Bool n
      | Dstring n  -> String n
      | Dloc n     -> raise Nonexpressible
      | Dfunval n  -> Funval n
      | Dprocval n -> raise Nonexpressible
      | Unbound    -> Novalue)
