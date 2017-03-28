(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

module type STACK =
  sig
    type 'a stack
    val emptystack : int * 'a -> 'a stack
    val push : 'a * 'a stack -> 'a stack
    val pop : 'a stack -> 'a stack
    val top : 'a stack -> 'a
    val empty : 'a stack -> bool
    val length : 'a stack  -> int
    exception Emptystack
    exception Fullstack
  end
