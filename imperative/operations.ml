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
let iszero x = if typecheck("int",x)
               then (match x with |Int(y) -> Bool(y=0)
                                  | _ -> failwith ("iszero match error"))
               else failwith ("iszero type error")

(* COMPUTE IF AN INT X IS EQUAL TO AN INT Y *)
let equ (x,y) = if typecheck("int",x) && typecheck("int",y)
                then (match (x,y) with |(Int(u), Int(w)) -> Bool(u=w)
                                       | _ -> failwith ("equ match error"))
                else failwith ("equ type error")

(* COMPUTE A SUM OF TWO INTS *)
let plus (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u+w)
                                        | _ -> failwith ("plus match error"))
                 else failwith ("plus type error")

(* COMPUTE A DIFFERNCE OF TWO INTS *)
let diff (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u-w)
                                        | _ -> failwith ("diff match error"))
                 else failwith ("diff type error")

(* COMPUTE A MULTIPLICATION OF TWO INTS *)
let mult (x,y) = if typecheck("int",x) && typecheck("int",y)
                 then (match (x,y) with |(Int(u), Int(w)) -> Int(u*w)
                                        | _ -> failwith ("mult match error"))
                 else failwith ("mult type error")

(* COMPUTE THE LOGIC OPERATION "AND" *)
let et (x,y) = if typecheck("bool",x) && typecheck("bool",y)
               then (match (x,y) with |(Bool(u), Bool(w)) -> Bool(u && w)
                                      | _ -> failwith ("et match error"))
               else failwith ("et type error")

(* COMPUTE THE LOGIC OPERATION "OR" *)
let vel (x,y) = if typecheck("bool",x) && typecheck("bool",y)
                then (match (x,y) with |(Bool(u), Bool(w)) -> Bool(u || w)
                                       | _ -> failwith ("vel match error"))
                else failwith ("vel type error")

(* COMPUTE THE LOGIC OPERATION "NOT" *)
let non x = if typecheck("bool",x)
            then (match x with |Bool(x) -> Bool(not(x))
                               | _ -> failwith ("non match error"))
            else failwith ("non type error")
(* --- BASIC FUNCTIONS - END --- *)
(* --- STRING FUNCTIONS - START --- *)
(* CONCATENATE STRING X TO STRING Y *)
let conc (x,y) = if typecheck("string",x) && typecheck("string",y)
                    then (match (x,y) with | (String(x), String(y)) -> String(String.concat "" [x; y])
                                       | _ -> failwith ("concat match error"))
                    else failwith ("concat type error")

(* CUT A STRING X AND FROM INDEX "i1" (included) TO INDEX "i2" (not included) *)
let subs (x,i1,i2) = if typecheck("string",x) && typecheck("int",i1) && typecheck("int",i2)
                        then (match (x,i1,i2) with | (String(x), Int(i1), Int(i2)) -> String(String.sub x i1 ((i2-i1)+1))
                                           | _ -> failwith ("subs match error"))
                        else failwith ("subs type error")

(* COMPUTE THE LENGTH OF THE STRING X *)
let len x = if typecheck("string",x)
                then (match x with | String(x) -> Int(String.length x)
                                   | _ -> failwith ("len match error"))
                else failwith ("len type error")

(* RETURN THE CHAR IN Y POSITION OF THE STRING X*)
let charat (x,y) = if typecheck("string",x) && typecheck("int",y)
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

(* Count of the occurence of a character in a string *)
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

(* Converter function *)
let str_to_int s =
  if typecheck("string",s) then
    (match (s) with String(u) -> int_of_string(u))
  else
    failwith ("type error or string not valid")

let str_to_bool s =
  if typecheck("string",s) then
    (match (s) with String(u) -> bool_of_string(u))
  else
    failwith ("type error or string not valid")

(* Top and Pop Stack Function *)
let topop s =
      let top = top(s) in let pop = pop(s) in top

(* Function parser for command Reflect *)
let rec parser (e,op_stack,st_stack) =
      match e with String(n) ->

      (* Base Case *)
          (* String is empty - then return it *)
          if isnull( len(String(n)) ) then Estring n

      (* Inductive Step *)

          (* Eint type *)
          else if eq_string( String(String.sub (n) 0 4), String("Eint")) then
              if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                (* If the operator is before , *)
                let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else if ((String.contains(n) ')')) then
                (* If the operator is after , *)
                let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else
                let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)))), op_stack) in
                topop(op_stack)

          (* Ebool type *)
          else if eq_string( String(String.sub (n) 0 5), String("Ebool")) then
              if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                (* If the operator is before , *)
                let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else if ((String.contains(n) ')')) then
                (* If the operator is after , *)
                let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else
                let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)))), op_stack) in
                topop(op_stack)

          (* Estring type *)
          else if eq_string( String(String.sub (n) 0 7), String("Estring")) then
              if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                (* If the operator is before , *)
                let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)), op_stack) in
                let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else if ((String.contains(n) ')')) then
                (* If the operator is after , *)
                let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)), op_stack) in
                let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else
                let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)), op_stack) in
                topop(op_stack)

          (* Ignored character *)

          (* "," character is ignored *)
          else if eq_string(subs(String(n),Int(0),Int(0)),String(",")) then
              parser( String(String.sub (n) 1 (((String.length) n)-1)),op_stack,st_stack )
          (* ")" character is ignored *)
          else if eq_string(subs(String(n),Int(0),Int(0)),String(")")) then
              parser( String(String.sub (n) 1 (((String.length) n)-1)),op_stack,st_stack )

          (* Opearations *)

          (*
           *
           *
           *)

          (* Opeator Minus *)
          else if eq_string(String(String.sub (n) 0 5), String("Minus")) then
              let i1 = push(parser(String(String.sub (n) 6 (((String.length) n)-6)),op_stack,st_stack), op_stack) in
              Minus(topop(op_stack))
          (* Operator Sum *)
          else if eq_string(String(String.sub (n) 0 3), String("Sum")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Sum(topop(op_stack),topop(op_stack))
          (* Operator Diff *)
          else if eq_string(String(String.sub (n) 0 4), String("Diff")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Diff(topop(op_stack),topop(op_stack))
          (* Operator Prod *)
          else if eq_string(String(String.sub (n) 0 4), String("Prod")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Prod(topop(op_stack),topop(op_stack))
          (* Opeator Iszero *)
          else if eq_string(String(String.sub (n) 0 6), String("Iszero")) then
              let i1 = push(parser(String(String.sub (n) 7 (((String.length) n)-7)),op_stack,st_stack), op_stack) in
              Iszero(topop(op_stack))
          (* Opeator Not *)
          else if eq_string(String(String.sub (n) 0 3), String("Not")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              Not(topop(op_stack))
          (* Operator And *)
          else if eq_string(String(String.sub (n) 0 3), String("And")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              And(topop(op_stack),topop(op_stack))
          (* Operator Or *)
          else if eq_string(String(String.sub (n) 0 2), String("Or")) then
              let i1 = push(parser(String(String.sub (n) 3 (((String.length) n)-3)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Or(topop(op_stack),topop(op_stack))
          (* Operator Eq *)
          else if eq_string(String(String.sub (n) 0 2), String("Eq")) then
              let i1 = push(parser(String(String.sub (n) 3 (((String.length) n)-3)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Eq(topop(op_stack),topop(op_stack))
          (* Opeator Len *)
          else if eq_string(String(String.sub (n) 0 3), String("Len")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              Len(topop(op_stack))
          (* Operator Conc *)
          else if eq_string(String(String.sub (n) 0 4), String("Conc")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Conc(topop(op_stack),topop(op_stack))
          (* Operator Streq *)
          else if eq_string(String(String.sub (n) 0 5), String("Streq")) then
              let i1 = push(parser(String(String.sub (n) 6 (((String.length) n)-6)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Streq(topop(op_stack),topop(op_stack))
          (* Operator Charat *)
          else if eq_string(String(String.sub (n) 0 6), String("Charat")) then
              let i1 = push(parser(String(String.sub (n) 7 (((String.length) n)-7)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Charat(topop(op_stack),topop(op_stack))
          (* Operator Subs *)
          else if eq_string(String(String.sub (n) 0 4), String("Subs")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              let i3 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Subs(topop(op_stack),topop(op_stack),topop(op_stack))


          else failwith ("parser error or command not found")
