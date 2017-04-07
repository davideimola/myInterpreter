(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

(* SUPPORT FUNCTION - TYPECHECK *)
let typecheck (x, y) = match x with
      | "int" -> (match y with
          | Int(u) -> true
          | _ -> false )
      | "bool" -> (match y with
          | Bool(u) -> true
          | _ -> false )
      | "string" -> (match y with
          | String(u) -> true
          | _ -> false )
      | _ -> failwith ("not a valid type")


(* --- BASIC FUNCTIONS - START --- *)
(* COMPUTE THE OPPOSITE OF A NUMBER *)
let minus x = if typecheck("int",x)
              then (match x with |Int(y) -> Int(-y)
                                 | _ -> failwith ("minus match error"))
              else failwith ("minus type error")

(* COMPUTE IF A NUMBER IS EQUAL TO ZERO *)
and iszero x = if typecheck("int",x)
               then (match x with |Int(y) -> Bool(y=0)
                                  | _ -> failwith ("iszero match error"))
               else failwith ("iszero type error")

(* COMPUTE IF AN INT X IS EQUAL TO AN INT Y *)
and equ (x,y) = if typecheck("int",x) && typecheck("int",y)
                then (match (x,y) with |(Int(u), Int(w)) -> Bool(u=w)
                                       | _ -> failwith ("equ match error"))
                else failwith ("equ type error")

(* COMPUTE A SUM OF TWO INTS *)
and plus (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u+w)
                                        | _ -> failwith ("plus match error"))
                 else failwith ("plus type error")

(* COMPUTE A DIFFERNCE OF TWO INTS *)
and diff (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u-w)
                                        | _ -> failwith ("diff match error"))
                 else failwith ("diff type error")

(* COMPUTE A MULTIPLICATION OF TWO INTS *)
and mult (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u*w)
                                        | _ -> failwith ("mult match error"))
                 else failwith ("mult type error")

(* COMPUTE THE LOGIC OPERATION "AND" *)
and et (x,y) = if typecheck("bool",x) && typecheck("bool",y)
               then (match (x,y) with |(Bool(u), Bool(w)) -> Bool(u && w)
                                      | _ -> failwith ("et match error"))
               else failwith ("et type error")

(* COMPUTE THE LOGIC OPERATION "OR" *)
and vel (x,y) = if typecheck("bool",x) && typecheck("bool",y)
                then (match (x,y) with |(Bool(u), Bool(w)) -> Bool(u || w)
                                       | _ -> failwith ("vel match error"))
                else failwith ("vel type error")

(* COMPUTE THE LOGIC OPERATION "NOT" *)
and non x = if typecheck("bool",x)
            then (match x with |Bool(x) -> Bool(not(x))
                               | _ -> failwith ("non match error"))
            else failwith ("non type error")
(* --- BASIC FUNCTIONS - END --- *)


(* --- STRING FUNCTIONS - START --- *)
(* CONCATENATE STRING X TO STRING Y *)
and conc (x,y) = if typecheck("string",x) && typecheck("string",y)
                    then (match (x,y) with | (String(x), String(y)) -> String(String.concat "" [x; y])
                                       | _ -> failwith ("conc match error"))
                    else failwith ("concat type error")

(* CUT A STRING X AND FROM INDEX "i1" (included) TO INDEX "i2" (not included) *)
and subs (x,i1,i2) = if typecheck("string",x) && typecheck("int",i1) && typecheck("int",i2)
                        then (match (x,i1,i2) with | (String(x), Int(i1), Int(i2)) -> String(String.sub x i1 (i2-i1))
                                           | _ -> failwith ("subs match error"))
                        else failwith ("substr type error")

(* COMPUTE THE LENGTH OF THE STRING X *)
and len x = if typecheck("string",x)
                then (match x with | String(x) -> Int(String.length x)
                                   | _ -> failwith ("leng match error"))
                else failwith ("sLength type error")

(* COMPARE IF THE TWO STRINGS ARE EQUALS *)
and streq (x,y) = if typecheck("string",x) && typecheck("string",y)
                  then (match (x,y) with | (String(x), String(y))-> Bool(String.equal x y)
                                         | _ -> failwith ("streq match error"))
                  else failwith ("streq type error")

(* CALL THE INTERPRETER ON THE STRING *)
and reflect x = if typecheck("string",x)
                then
                else failwith ("Reflect type error")
(* --- STRING FUNCTIONS - END --- *)

let rec parser (x,s1,s2) =
  if typecheck("string",x)
  then
  else
