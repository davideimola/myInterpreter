(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)


type eval =
        | Int of int
        | Bool of bool
        | String of string
        | Funval of efun
        | Novalue
and dval =
        | Dint of int
        | Dbool of bool
        | Unbound
        | Dloc of loc
        | Dfunval of efun
        | Dprocval of proc
and mval =
        | Mint of int
        | Mbool of bool
        | Undefined
and efun = (dval list) * (mval store) -> eval
and proc = (dval list) * (mval store) -> mval store
