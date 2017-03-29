(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

#use "syntax.ml";;

#use "env/env_interface.ml";;
#use "env/env_semantics.ml";;
open Funenv;;

#use "stack/stack_interface.ml";;
#use "stack/stack_semantics.ml";;
open SemStack;;

#use "stack/stackm_interface.ml";;
#use "stack/stackm_semantics.ml";;
open SemStack_Modificable;;

#use "store/store_interface.ml";;
#use "store/store_semantics.ml";;
open Funstore;;

#use "domains.ml";;
#use "operations.ml";;
#use "semantics.ml";;
