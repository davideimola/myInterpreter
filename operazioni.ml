(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

let typecheck (x,y) = match x with
      | "int" -> (match y with
          | Int(u) -> true
          | _ -> false )
      | "bool" -> (match y with
          | Bool(u) -> true
          | _ -> false )
      | _ -> failwith ("not a valid type")

and minus x = if typecheck("int",x)
              then (match x with Int(y) -> Int(-y) )
              else failwith ("type error")

and iszero x = if typecheck("int",x)
               then (match x with Int(y) -> Bool(y=0) )
               else failwith ("type error")

and equ (x,y) = if typecheck("int",x) && typecheck("int",y)
                then (match (x,y) with (Int(u), Int(w)) -> Bool(u=w))
                else failwith ("type error")

and plus (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with (Int(u), Int(w)) -> Int(u+w))
                 else failwith ("type error")

and diff (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with (Int(u), Int(w)) -> Int(u-w))
                 else failwith ("type error")

and mult (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with (Int(u), Int(w)) -> Int(u*w))
                 else failwith ("type error")

and et (x,y) = if typecheck("bool",x) && typecheck("bool",y)
               then (match (x,y) with (Bool(u), Bool(w)) -> Bool(u && w))
               else failwith ("type error")

and vel (x,y) = if typecheck("bool",x) && typecheck("bool",y)
                then (match (x,y) with (Bool(u), Bool(w)) -> Bool(u or w))
                else failwith ("type error")

and non (x,y) = if typecheck("bool",x)
                then (match x with Bool(y) -> Bool(not y))
                else failwith ("type error")
