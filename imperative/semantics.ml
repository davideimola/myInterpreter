(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

(* SEMANTICS: LINK EVERY FUNCTION TAG WITH THE CORRECT OPERATION *)
let rec sem (e:exp) (r:dval env) (s: mval store) =
      match e with
      | Eint(n)    -> Int(n)
      | Ebool(b)   -> Bool(b)
      | Estring(s) -> String(s)
      | Den(i)     -> dvaltoeval(applyenv(r,i))
      | Iszero(a)  -> iszero((sem a r s) )
      | Eq(a,b)    -> equ((sem a r s) ,(sem b r s) )
      | Prod(a,b)  -> mult ( (sem a r s), (sem b r s) )
      | Sum(a,b)   -> plus ( (sem a r s), (sem b r s) )
      | Diff(a,b)  -> diff ( (sem a r s), (sem b r s) )
      | Minus(a)   -> minus( (sem a r s) )
      | And(a,b)   -> et ( (sem a r s), (sem b r s) )
      | Or(a,b)    -> vel ( (sem a r s), (sem b r s) )
      | Not(a)     -> non( (sem a r s) )
      | Ifthenelse(a,b,c) -> let g = sem a r s in
            if typecheck("bool",g) then (if g = Bool(true) then sem b r s else sem c r s)
            else failwith ("nonboolean guard")
      | Val(e) -> let (v, s1) = semden e r s in
                                (match v with
                                | Dloc n -> mvaltoeval(applystore(s1, n))
                                | _      -> failwith("not a variable"))
      | Let(i,e1,e2) -> let (v, s1) =
                              semden e1 r s in
                                    sem e2 (bind (r ,i, v)) s1
      | Fun(i,e1)   -> dvaltoeval(makefun(e,r))
      | Rec(i,e1)   -> makefunrec(i, e1, r)
      | Appl(a,b)   -> let (v1, s1) =
                          semlist b r s in
                                applyfun(evaltodval(sem a r s), v1, s1)

      | Len(a)        -> len( (sem a r s) )
      | Conc(a,b)     -> conc( (sem a r s),(sem b r s) )
      | Streq(a,b)    -> streq( (sem a r s),(sem b r s) )
      | Charat(a,b)   -> charat( (sem a r s),(sem b r s) )
      | Subs(a,i1,i2) -> subs( (sem a r s),(sem i1 r s),(sem i2 r s) )

      | _             -> failwith ("nonlegal expression for sem")

and makefun ((a:exp),(x:dval env)) =
      (match a with
      | Fun(ii,aa) -> Dfunval(function (d, s) -> sem aa (bindlist (x, ii, d)) s)
      | _          -> failwith ("Non-functional object"))

and makefunrec (i, Fun(ii, aa), r) =
      let functional ff (d,s1) =
            let r1 = bind(bindlist(r, ii, d), i, Dfunval(ff)) in
                  sem aa r1 s1 in
                  let rec fix = function x -> functional fix x in Funval(fix)

and makeproc ((a:exp),(x:dval env)) = match a with
      | Proc(ii,b) -> Dprocval(function (d, s) -> semb b (bindlist (x, ii, d)) s)
      | _          -> failwith ("Non-functional object")

and applyfun ((ev1:dval),(ev2:dval list), s) =
      ( match ev1 with
      | Dfunval(x) -> x (ev2, s)
      | _          -> failwith ("attempt to apply a non-functional object"))

and applyproc ((ev1:dval),(ev2:dval list), s) = match ev1 with
      | Dprocval(x) -> x (ev2, s)
      | _           -> failwith ("attempt to apply a non-functional object")

and semden (e:exp) (r:dval env) (s: mval store) =
      match e with
      | Den(i)     -> (applyenv(r,i), s)
      | Fun(i, e1) -> (makefun(e, r), s)
      | Proc(i, b) -> (makeproc(e, r), s)
      | Newloc(e)  -> let m = evaltomval(sem e r s) in let (l, s1) = allocate(s, m) in (Dloc l, s1)
      | _          -> (evaltodval(sem e r s), s)

and semlist el r s =
      match el with
      | []     -> ([], s)
      | e::el1 -> let (v1, s1) = semden e r s in let (v2, s2) = semlist el1 r s1 in (v1 :: v2, s2)

and semc (c: com) (r:dval env) (s: mval store) = match c with
      | Assign(e1, e2) -> let (v1, s1) = semden e1 r s in
                                         (match v1 with
                                         | Dloc(n) -> update(s1, n, evaltomval(sem e2 r s))
                                         | _ -> failwith ("wrong location in assignment"))
      | Cifthenelse(e, cl1, cl2) -> let g = sem e r s in
                                            if typecheck("bool",g) then
                                                  (if g = Bool(true) then semcl cl1 r s else semcl cl2 r s)
                                            else failwith ("nonboolean guard")
      | While(e, cl) ->
                let functional ((fi: mval store -> mval store)) =
                        function sigma ->
                            let g = sem e r sigma in
                                    if typecheck("bool",g) then
                                        (if g = Bool(true) then fi(semcl cl r sigma) else sigma)
                                    else failwith ("nonboolean guard")
                in
                let rec ssfix = function x -> functional ssfix x in ssfix(s)
      | Call(e1, e2) -> let (p, s1) = semden e1 r s in let (v, s2) = semlist e2 r s1 in applyproc(p, v, s2)
      | Block(b) -> semb b r s

      (* FUNCTION FOR VALUATE AN INPUT STRING e *)
      | Reflect(e) -> let g = sem e r s in
        (* CHECK THE FIRST TOKEN:
           IF ITS A VALID COMMAND             -> EXECUTE THE FIRST BLOCK  (parserCom)
           IF ITS NOT A COMMAND BUT IS VALID  -> EXECUTE THE SECOND BLOCK (parser)
           OTHERWISE                          -> FAIL BECAUSE STRING IS NOT VALID
        *)
        (* FIRST BLOCK - parserCom *)
        if typecheck("string",g) && eq_int(occurrence(g,String("(")),occurrence(g,String(")"))) && len(g)>=Int(5) && isCommand(g)
          then let st_stack = emptystack(100,Novalue) in         (* String Stack*)
               let op_stack = emptystack(100,Undefinedstack) in  (* Operation Stack*)
               let com = parserCom(g,op_stack,st_stack) in
               semc com r s
        (* SECOND BLOCK - parser *)
        else if typecheck("string",g) && eq_int(occurrence(g,String("(")),occurrence(g,String(")"))) && len(g)>=Int(5)
          then let st_stack = emptystack(100,Novalue) in         (* String Stack*)
               let op_stack = emptystack(100,Undefinedstack) in  (* Operation Stack*)
               let exp = parser(g,op_stack,st_stack) in
               (* If String stack is empty the result is OK *)
               if empty(st_stack) || eq_string(subs(top(st_stack),Int(0),Int(0)),String(")"))
                   then semc (Assign(Den "result",exp)) r s
                   else failwith ("parser error")
        (* OTHERWISE *)
        else failwith ("string not valid")

and semcl cl r s = match cl with
      | [] -> s
      | c::cl1 -> semcl cl1 r (semc c r s)

and semb (dl, rdl, cl) r s =
      let (r1, s1) = semdl (dl, rdl) r s in semcl cl r1 s1

and semdl (dl, rl) r s = let (r1, s1) = semdv dl r s in
                       semdr rl r1 s1

and semdv dl r s = match dl with
      | [] -> (r,s)
      | (i,e)::dl1 -> let (v, s1) = semden e r s in semdv dl1 (bind(r, i, v)) s1

and semdr rl r s =
      let functional ((r1: dval env)) = (match rl with
            | [] -> r
            | (i,e) :: rl1 -> let (v, s2) = semden e r1 s in
                  let (r2, s3) = semdr rl1 (bind(r, i, v)) s in r2) in
                        let rec rfix = function x -> functional x in (r, s)
