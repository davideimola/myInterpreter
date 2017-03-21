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
        | Unbound
and efun = eval list -> eval
