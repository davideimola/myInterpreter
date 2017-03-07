(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)
module type STORE =
    sig
      type 't store
      type loc
      val emptystore : 't -> 't store
      val allocate : 't store * 't -> loc * 't store
      val update : 't store * loc * 't -> 't store
      val applystore : 't store * loc -> 't
    end
