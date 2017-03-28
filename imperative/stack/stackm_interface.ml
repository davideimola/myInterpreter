(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

module type STACK_MODIFICABLE =
  sig
    type 'a stack
    val emptystack : int * 'a -> 'a stack
    val push       : 'a * 'a stack -> unit
    val pop        : 'a stack -> unit
    val top        : 'a stack -> 'a
    val empty      : 'a stack -> bool
    val length     : 'a stack -> int
    val clear      : 'a stack -> unit
    val access     : 'a stack * int -> 'a
    exception Emptystack
    exception Fullstack
    exception Wrongaccess
  end
