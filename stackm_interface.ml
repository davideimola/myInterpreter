(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

# module type MYSTACKM
    sig
      type 'a stack
      val emptystack : int * 'a -> 'a stack
      val push : 'a * 'a stack -> 'a stack
      val pop : 'a stack -> 'a stack
      val top : 'a stack -> 'a
      val empty : 'a stack -> bool
      val leng : 'a stack -> int
      val clears : 'a stack -> 'a stack
      val access : 'a stack * int -> 'a
      exception Emptystack
      exception Fullstack
      exception Wrongaccess
    end
