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
                                       | _ -> failwith ("concat match error"))
                    else failwith ("concat type error")

(* CUT A STRING X AND FROM INDEX "i1" (included) TO INDEX "i2" (not included) *)
and subs (x,i1,i2) = if typecheck("string",x) && typecheck("int",i1) && typecheck("int",i2)
                        then (match (x,i1,i2) with | (String(x), Int(i1), Int(i2)) -> String(String.sub x i1 ((i2-i1)+1))
                                           | _ -> failwith ("substr match error"))
                        else failwith ("substr type error")

(* COMPUTE THE LENGTH OF THE STRING X *)
and len x = if typecheck("string",x)
                then (match x with | String(x) -> Int(String.length x)
                                   | _ -> failwith ("len match error"))
                else failwith ("len type error")

(* RETURN THE CHAR IN Y POSITION OF THE STRING X*)
and charat (x,y) = if typecheck("string",x) && typecheck("int",y)
                   then (match (x,y) with | (String(x), Int(y)) -> String(String.sub x y 1)
                                          | _ -> failwith ("charat match error"))
                   else failwith ("charat type error")

(* COMPARE IF THE TWO STRINGS ARE EQUALS *)
let streq (x,y) = if typecheck("string",x) && typecheck("string",y)
                  then (match (x,y) with | (String(x), String(y)) -> iszero(Int(String.compare (x) y))
                                         | _ -> failwith ("streq match error"))
                  else failwith ("streq type error")
(* --- STRING FUNCTIONS - END --- *)

(* --- OPERATIONS FUNCTIONS - START --- *)
let isnull x = if typecheck("int",x)
              then (match x with |Int(y) -> (y=0)
                                  | _ -> failwith ("isnull match error"))
              else failwith ("isnull type error")

let eq_string (x,y) = if typecheck("string",x) && typecheck("string",y)
                  then (match (x,y) with | (String(x), String(y)) -> isnull(Int(String.compare (x) y))
                                          | _ -> failwith ("eq_string match error"))
                  else failwith ("eq_string type error")

let eq_int (x,y) = if typecheck("int",x) && typecheck("int",y)
                   then (match (x,y) with |(Int(u), Int(w)) -> (u=w)
                                          | _ -> failwith ("eq_int match error"))
                   else failwith ("eq_int type error")
(* --- OPERATIONS FUNCTIONS - END --- *)

(* --- FUNCTIONS FOR REFLECT - START --- *)

(* FUCNTION GREATER-EQUAL *)

let occurrence (x,y) =
  if typecheck("string",x) && typecheck("string",y)
  then (
    match (x,y) with (String(x),String(y)) ->
      let tmp = 0 in
      let i = 0 in
      let x_len = len(String(x)) in
      let rec loop ( Int(i),Int(tmp) ) =
        if ( Int(i)>=x_len )
        then Int(tmp)
        else if ( eq_string( charat(String(x),Int(i)), String(y)) ) then
          loop ( plus(Int(i),Int(1)), plus(Int(tmp),Int(1)) )
        else
          loop ( plus(Int(i),Int(1)), Int(tmp) )
      in loop ( Int(i),Int(tmp) )
  )
  else failwith ("occurrence type error")
(*
let parser (e,op_stack,st_stack) =
      match e with String(n) ->

          (* Base Case *)
          (* String is empty - then return it *)
          if isnull( len(n) ) then Estring n

          (* Inductive Step *)

          else if eq_string( String(charat( n, 0 )), String(",")) then                   (* "," char is ignored *)
              parser( String(subs n 1 (len(n)-1)) ,op_stack,st_stack )
          else if eq_string( String(charat( n, 0 )), String(")") ) then                  (* ")" char is ignored *)
              parser( String(subs (n) 1 (len(n)-1)) ,op_stack,st_stack )
          else if eq_string( String(subs (n) 0 3), String("Sum")) then
*)

(* == PROVA == *)

(* search in a string s for a char c and return that char index*)
let find (s, c) =
    if typecheck("string",s) && typecheck("string",c)
    then (
      match (s,c) with
        |(String(str),String(ch)) ->
            let i = 0 in
            let open_brack = 0 in
            let str_len = len(String(str)) in
            let rec loop ( Int(i), Int(open_brack) ) =
               if (Int(i)>=str_len) || (Int(open_brack)<Int(0)) then
               (* all the string is scanned then char is not found, error *)
               (* open_brack is negative then I have a more ")" then "(" , error *)
                   failwith ("string costruction error")
               (* found the char and var open_brack is 0, then return char index and finish scan *)
               else if (eq_string(charat(String(str),Int(i)), String(ch))) && (eq_int(Int(open_brack),Int(0))) then
                   Int(i)
               (* searched char not found, then control for brackets and scan next char*)
               else if ( eq_string( charat(String(str),Int(i)), String("(")) ) then
                   loop (plus(Int(i),Int(1)), plus(Int(open_brack),Int(1)))
               else if ( eq_string( charat(String(str),Int(i)), String(")")) ) then
                   loop (plus(Int(i),Int(1)), diff(Int(open_brack),Int(1)))
               (* no char or brackets found, then scan next char*)
               else loop (plus(Int(i),Int(1)), Int(open_brack))
          in loop ( Int(i), Int(open_brack) )
      | _ -> failwith ("find match error")
    )
    else failwith ("find type error")



let rec parser (e,op_stack,st_stack) =
      match e with String(n) ->

          (* Base Case *)
          (* String is empty - then return it *)
          if isnull( len(String(n)) ) then Estring n

          (* Inductive Step *)

          else if eq_string( charat( String(n), Int(0) ), String(",")) then                   (* "," char is ignored *)
              parser(subs(String(n), Int(1) , diff(len(String(n)),Int(1))), op_stack, st_stack)
          else if eq_string( charat( String(n), Int(0) ), String(")")) then                   (* ")" char is ignored *)
              parser(subs(String(n), Int(1) , diff(len(String(n)),Int(1))), op_stack, st_stack)
          else if eq_string( subs(String(n),Int(0),Int(3)), String("Sum")) then
              (
               let src1 = find(subs(String(n),Int(4),diff(len(String(n)),Int(1))), String(",")) in
               let src2 = find(subs(String(n),src1,diff(len(String(n)),Int(1))), String(")")) in
               let p1 = parser( subs(String(n),Int(1),src1) ,op_stack,st_stack ) in
               let p2 = parser( subs(String(n),src1,src2) ,op_stack,st_stack ) in
               Sum(p1,p2)
              )
          else if eq_string( subs(String(n),Int(0),Int(4)), String("Eint")) then
               Eint(int_of_string (String.sub n 4 ((String.length n)-1)))
          else failwith("command not found")

(* --- FUNCTIONS FOR REFLECT - END --- *)
