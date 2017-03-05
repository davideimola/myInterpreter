(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

# module type MYSTACK
    sig
      type 'a stack
      val emptystack : int * 'a -> 'a stack
      val push : 'a * 'a stack -> 'a stack
      val pop : 'a stack -> 'a stack
      val top : 'a stack -> 'a
      val empty : 'a stack -> bool
      val leng : 'a stack  -> int
      exception Emptystack
      exception Fullstack
    end
