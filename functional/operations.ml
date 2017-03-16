(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

let typecheck (x, y) = match x with
      | "int" -> (match y with
          | Int(u) -> true
          | _ -> false )
      | "bool" -> (match y with
          | Bool(u) -> true
          | _ -> false )
      | _ -> failwith ("not a valid type")

let minus x = if typecheck("int",x)
              then (match x with |Int(y) -> Int(-y)
                                 | _ -> failwith ("minus match error"))
              else failwith ("minus type error")

and iszero x = if typecheck("int",x)
               then (match x with |Int(y) -> Bool(y=0)
                                  | _ -> failwith ("iszero match error"))
               else failwith ("iszero type error")

and equ (x,y) = if typecheck("int",x) && typecheck("int",y)
                then (match (x,y) with |(Int(u), Int(w)) -> Bool(u=w)
                                       | _ -> failwith ("equ match error"))
                else failwith ("equ type error")

and plus (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u+w)
                                        | _ -> failwith ("plus match error"))
                 else failwith ("plus type error")

and diff (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u-w)
                                        | _ -> failwith ("diff match error"))
                 else failwith ("diff type error")

and mult (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u*w)
                                        | _ -> failwith ("mult match error"))
                 else failwith ("mult type error")

and et (x,y) = if typecheck("bool",x) && typecheck("bool",y)
               then (match (x,y) with |(Bool(u), Bool(w)) -> Bool(u && w)
                                      | _ -> failwith ("et match error"))
               else failwith ("et type error")

and vel (x,y) = if typecheck("bool",x) && typecheck("bool",y)
                then (match (x,y) with |(Bool(u), Bool(w)) -> Bool(u || w)
                                       | _ -> failwith ("vel match error"))
                else failwith ("vel type error")

and non x = if typecheck("bool",x)
                then (match x with |Bool(x) -> Bool(not(x))
                                   | _ -> failwith ("non match error"))
                else failwith ("non type error")

and sLength x = if typecheck("string",x)
                then calcola la lunghezza
                else failwith ("sLength type error")

and concat (x,y) = if typecheck("string",x) && typecheck("string",y)
                   then concatena due stringhe
                   else failwith ("concat type error")

and substr (x,i1,i2) = if typecheck("string",x) && typecheck("int",i1) && typecheck("int",i2)
                      then cattura il pezzo interessato
                      else failwith ("substr type error")
