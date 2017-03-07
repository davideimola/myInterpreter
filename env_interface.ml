(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

module type ENV =
  sig
    type 't env
    val emptyenv : 't -> 't env
    val bind : 't env * string * 't -> 't env
    val bindlist : 't env * (string list) * ('t list) -> 't env
    val applyenv : 't env * string -> 't
    exception WrongBindlist
  end
