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

(* COMPARE IF THE TWO STRINGS ARE eq_string *)
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

(* Top and Pop op_stack Function *)
let topop s =
      let top = top(s) in let pop = pop(s) in top

let isCommand s =
      match s with String(g) ->
          if eq_string(subs(String(g),Int(0),Int(5)),String("Assign")) ||
             eq_string(subs(String(g),Int(0),Int(10)),String("Cifthenelse")) ||
             eq_string(subs(String(g),Int(0),Int(4)),String("While")) ||
             eq_string(subs(String(g),Int(0),Int(3)),String("Call")) ||
             eq_string(subs(String(g),Int(0),Int(6)),String("Reflect")) then
            true
          else
            false


(* Function parser for command Reflect for functions *)
let rec parser (e,op_stack,st_stack) =
      match e with String(n) ->

      (* Base Case *)
          (* String is empty - then return it *)
          if isnull( len(String(n)) ) then Estring n
          else if eq_string(String(n),String(")")) then
            Estring n
      (* Inductive Step *)
          (* Ignored characters *)

          (* "," character is ignored *)
          else if eq_string(subs(String(n),Int(0),Int(0)),String(",")) then
              parser( String(String.sub (n) 1 (((String.length) n)-1)),op_stack,st_stack )
          (* ")" character is ignored *)
          else if eq_string(subs(String(n),Int(0),Int(0)),String(")")) && (String.length(n)!=1) then
              parser( String(String.sub (n) 1 (((String.length) n)-1)),op_stack,st_stack )


          (* Terminal types [Den, Eint, Ebool, Estring]
           * If a terminal type is recognized:
           * First block: check if is an operation first operator, before a char "," and a char ")" -> otherwise run second block
           * Second block: check if is an operation second operator, before only char ")" -> otherwise run third block
           * Third block: is alone
           *)


           (* Den type *)
           else if eq_string( String(String.sub (n) 0 3), String("Den")) then
               if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                 (* Den: first block *)
                 let i = push(Den(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)), op_stack) in
                 let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                 topop(op_stack)
               else if ((String.contains(n) ')')) then
                 (* Den: second block *)
                 let i = push(Den(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)), op_stack) in
                 let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                 topop(op_stack)
               else
                 (* Den: third block *)
                 let i = push(Den(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)), op_stack) in
                 topop(op_stack)
          (* Eint type *)
          else if eq_string( String(String.sub (n) 0 4), String("Eint")) then
              if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                (* Eint: first block *)
                let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else if ((String.contains(n) ')')) then
                (* Eint: second block *)
                let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else
                (* Eint: third block *)
                let i = push(Eint(str_to_int(String(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)))), op_stack) in
                topop(op_stack)
          (* Ebool type *)
          else if eq_string( String(String.sub (n) 0 5), String("Ebool")) then
              if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                (* Ebool: first block *)
                let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else if ((String.contains(n) ')')) then
                (* Ebool: second block *)
                let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)))), op_stack) in
                let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else
                (* Ebool: third block *)
                let i = push(Ebool(str_to_bool(String(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)))), op_stack) in
                topop(op_stack)
          (* Estring type *)
          else if eq_string( String(String.sub (n) 0 7), String("Estring")) then
              if ((String.contains(n) ',') && ((String.index(n) ',')<(String.index(n) ')'))) then
                (* Estring: first block *)
                let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ',')-(String.index(n) ' ')-1)), op_stack) in
                let j = push(subs(String(n),Int((String.index(n) ',')+1),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else if ((String.contains(n) ')')) then
                (* Estring: second block *)
                let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.index(n) ')')-(String.index(n) ' ')-1)), op_stack) in
                let j = push(subs(String(n),Int(String.index(n) ')'),diff(len(String(n)),Int(1))), st_stack) in
                topop(op_stack)
              else
                (* Estring: third block *)
                let i = push(Estring(String.sub (n) ((String.index(n) ' ')+1) ((String.length (n))-(String.index(n) ' ')-1)), op_stack) in
                topop(op_stack)

          (* Opearations *)

          (* If an operation type is recognized:
           * i1 -> push in op_stack the first terminal type. That is obtained from a recursive parser call.
           * i2 -> push in op_stack the second terminal type. (Needed only if the operation requires 2 or more parameters)
           * i3 -> push in op_stack the third terminal type. (Needed only if the operation requires 3 or more parameters)
           * Call the correct operation and take the parameters from the top of op_stack
           *)

          (* Operation Minus *)
          else if eq_string(String(String.sub (n) 0 5), String("Minus")) then
              let i1 = push(parser(String(String.sub (n) 6 (((String.length) n)-6)),op_stack,st_stack), op_stack) in
              Minus(topop(op_stack))
          (* Operation Sum *)
          else if eq_string(String(String.sub (n) 0 3), String("Sum")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Sum(topop(op_stack),topop(op_stack))
          (* Operation Diff *)
          else if eq_string(String(String.sub (n) 0 4), String("Diff")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Diff(topop(op_stack),topop(op_stack))
          (* Operation Prod *)
          else if eq_string(String(String.sub (n) 0 4), String("Prod")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Prod(topop(op_stack),topop(op_stack))
          (* Operation Iszero *)
          else if eq_string(String(String.sub (n) 0 6), String("Iszero")) then
              let i1 = push(parser(String(String.sub (n) 7 (((String.length) n)-7)),op_stack,st_stack), op_stack) in
              Iszero(topop(op_stack))
          (* Operation Not *)
          else if eq_string(String(String.sub (n) 0 3), String("Not")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              Not(topop(op_stack))
          (* Operation And *)
          else if eq_string(String(String.sub (n) 0 3), String("And")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              And(topop(op_stack),topop(op_stack))
          (* Operation Or *)
          else if eq_string(String(String.sub (n) 0 2), String("Or")) then
              let i1 = push(parser(String(String.sub (n) 3 (((String.length) n)-3)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Or(topop(op_stack),topop(op_stack))
          (* Operation Eq *)
          else if eq_string(String(String.sub (n) 0 2), String("Eq")) then
              let i1 = push(parser(String(String.sub (n) 3 (((String.length) n)-3)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Eq(topop(op_stack),topop(op_stack))
          (* Operation Val *)
          else if eq_string(String(String.sub (n) 0 3), String("Val")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              Val(topop(op_stack))
          (* Operation Newloc *)
          else if eq_string(String(String.sub (n) 0 6), String("Newloc")) then
              let i1 = push(parser(String(String.sub (n) 7 (((String.length) n)-7)),op_stack,st_stack), op_stack) in
              Newloc(topop(op_stack))
          (* Operation Appl *)
          else if eq_string(String(String.sub (n) 0 4), String("Appl")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = parserList(topop(st_stack),op_stack,st_stack) in
              Appl(topop(op_stack),i2)
          (* Operation Fun *)
          else if eq_string(String(String.sub (n) 0 3), String("Fun")) then
              let i1 = (String.sub (n) ((String.index(n) '[')+1) ((String.index(n) ']')-1)) in
              let l  = push(String(String.sub (n) ((String.index(n) ']')+1) (((String.length) n)-((String.index(n) ']')+1))), st_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Fun([i1],topop(op_stack))
          (* Operation Rec *)
          else if eq_string(String(String.sub (n) 0 3), String("Rec")) then
              let i1 = (String.sub (n) 4 (((String.index(n) ',')-4)-1)) in
              let l  = push(String(String.sub (n) (String.index(n) ',') (((String.length) n)-(String.index(n) ','))), st_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Rec(i1,topop(op_stack))
          (* Operation Ifthenelse *)
          else if eq_string(String(String.sub (n) 0 10),String("Ifthenelse")) then
              let i1 = push(parser(String(String.sub (n) 11 (((String.length) n)-11)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              let i3 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Ifthenelse(topop(op_stack),topop(op_stack),topop(op_stack))
          (* Operation Let *)
          else if eq_string(String(String.sub (n) 0 3),String("Let")) then
              let i1 = (String.sub (n) 4 (((String.index(n) ',')-4)-1)) in
              let l  = push(String(String.sub (n) (String.index(n) ',') (((String.length) n)-(String.index(n) ','))), st_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              let i3 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Let(i1,topop(op_stack),topop(op_stack))
          (* Operation Len *)
          else if eq_string(String(String.sub (n) 0 3), String("Len")) then
              let i1 = push(parser(String(String.sub (n) 4 (((String.length) n)-4)),op_stack,st_stack), op_stack) in
              Len(topop(op_stack))
          (* Operation Conc *)
          else if eq_string(String(String.sub (n) 0 4), String("Conc")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Conc(topop(op_stack),topop(op_stack))
          (* Operation Streq *)
          else if eq_string(String(String.sub (n) 0 5), String("Streq")) then
              let i1 = push(parser(String(String.sub (n) 6 (((String.length) n)-6)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Streq(topop(op_stack),topop(op_stack))
          (* Operation Charat *)
          else if eq_string(String(String.sub (n) 0 6), String("Charat")) then
              let i1 = push(parser(String(String.sub (n) 7 (((String.length) n)-7)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Charat(topop(op_stack),topop(op_stack))
          (* Operation Subs *)
          else if eq_string(String(String.sub (n) 0 4), String("Subs")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              let i3 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Subs(topop(op_stack),topop(op_stack),topop(op_stack))

          (* No Expression recognized *)
          else failwith ("parser error or expression not found")

and parserList (e,op_stack,st_stack) =
      match e with String(n) ->
      let listFunction = [] in
      if (String.contains(n) '[') && ((String.index(n) '[')<(String.index(n) ';')) then
          let i1 = (String.sub (n) ((String.index(n) '[')+1) (((String.length) n)-(String.index(n) '[')-1)) in
          let i2 = push(String(i1),st_stack) in
          let l1 = parser(String(i1),op_stack,st_stack) in
          let l2 = parserList(topop(st_stack),op_stack,st_stack) in
          let l = l1 :: l2 in l
      else if (String.contains(n) ';') && ((String.index(n) ';')<(String.index(n) ']')) then
          let i1 = (String.sub (n) ((String.index(n) ';')+1) (((String.length) n)-(String.index(n) ';')-1)) in
          let i2 = push(String(i1),st_stack) in
          let l1 = parser(String(i1),op_stack,st_stack) in
          let l2 = parserList(topop(st_stack),op_stack,st_stack) in
          let l = l1 :: l2 in l
      else
          let i1 = (String.sub (n) ((String.index(n) ']')+1) (((String.length) n)-(String.index(n) ']')-1)) in
          let i2 = push(String(i1),st_stack) in
          listFunction



(* Function parser for command Reflect for commands *)
let rec parserCom (e,op_stack,st_stack) =
      match e with String(n) ->
          (* "," character is ignored *)
          if eq_string(subs(String(n),Int(0),Int(0)),String(",")) then
            parserCom(String(String.sub (n) 1 (((String.length) n)-1)),op_stack,st_stack)
          (* ")" character is ignored *)
          else if eq_string(subs(String(n),Int(0),Int(0)),String(")")) && (String.length(n)!=1) then
            parserCom(String(String.sub (n) 1 (((String.length) n)-1)),op_stack,st_stack)

          (* Command Assign *)
          else if (((String.length) n)>=6) && eq_string(String(String.sub (n) 0 6),String("Assign")) then
              let i1 = push(parser(String(String.sub (n) 7 (((String.length) n)-7)),op_stack,st_stack), op_stack) in
              let i2 = push(parser(topop(st_stack),op_stack,st_stack), op_stack) in
              Assign(topop(op_stack),topop(op_stack))
          (* Command Block *)
          (* Command Cifthenelse *)
          else if (((String.length) n)>=11) && eq_string(String(String.sub (n) 0 11),String("Cifthenelse")) then
              let i1 = push(parser(String(String.sub (n) 12 (((String.length) n)-12)),op_stack,st_stack), op_stack) in
              let i2 = parserComList(topop(st_stack),op_stack,st_stack) in
              let i3 = parserComList(topop(st_stack),op_stack,st_stack) in
              Cifthenelse(topop(op_stack),i2,i3)
          (* Command Call *)
          else if (((String.length) n)>=4) && eq_string(String(String.sub (n) 0 4),String("Call")) then
              let i1 = push(parser(String(String.sub (n) 5 (((String.length) n)-5)),op_stack,st_stack), op_stack) in
              let i2 = parserList(topop(st_stack),op_stack,st_stack) in
              Call(topop(op_stack),i2)
          (* Command Reflect *)
          else if (((String.length) n)>=7) && eq_string(String(String.sub (n) 0 7),String("Reflect")) then
              let i1 = Estring(String.sub (n) 8 (((String.length) n)-8)) in
              Reflect(i1)
          (* Command While *)
          else if (((String.length) n)>=5) && eq_string(String(String.sub (n) 0 5),String("While")) then
              let i1 = push(parser(String(String.sub (n) 6 (((String.length) n)-6)),op_stack,st_stack), op_stack) in
              let i2 = parserComList(topop(st_stack),op_stack,st_stack) in
              While(topop(op_stack),i2)

          (* No command recognized *)
          else failwith ("parser error or command not found")

and parserComList (e,op_stack,st_stack) =
      match e with String(n) ->
          let listCommand = [] in
          if (String.contains(n) '[') && ((String.index(n) '[')<(String.index(n) ';')) then
              let i1 = (String.sub (n) ((String.index(n) '[')+1) (((String.length) n)-(String.index(n) '[')-1)) in
              let i2 = push(String(i1),st_stack) in
              let l1 = parserCom(String(i1),op_stack,st_stack) in
              let l2 = parserComList(topop(st_stack),op_stack,st_stack) in
              let l = l1 :: l2 in l
          else if (String.contains(n) ';') && ((String.index(n) ';')<(String.index(n) ']')) then
              let i1 = (String.sub (n) ((String.index(n) ';')+1) (((String.length) n)-(String.index(n) ';')-1)) in
              let i2 = push(String(i1),st_stack) in
              let l1 = parserCom(String(i1),op_stack,st_stack) in
              let l2 = parserComList(topop(st_stack),op_stack,st_stack) in
              let l = l1 :: l2 in l
          else
              let i1 = (String.sub (n) ((String.index(n) ']')+1) (((String.length) n)-(String.index(n) ']')-1)) in
              let i2 = push(String(i1),st_stack) in
              listCommand
