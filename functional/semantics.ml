(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

let rec sem (e:exp) (r:eval env) =
      match e with
      | Eint(n) -> Int(n)
      | Ebool(b) -> Bool(b)
      | Den(i) -> applyenv(r,i)
      | Iszero(a) -> iszero((sem a r))
      | Eq(a,b) -> equ((sem a r),(sem b r))
      | Prod(a,b) -> mult((sem a r),(sem b r))
      | Sum(a,b) -> plus((sem a r),(sem b r))
      | Minus(a) -> minus((sem a r))
      | And(a,b) -> et((sem a r),(sem b r))
      | Or(a,b) -> vel((sem a r),(sem b r))
      | Not(a) -> non((sem a r))
      | Ifthenelse(a,b,c) -> let g = sem a r in
          if typecheck("bool",g)
          then (if g = Bool(true)
                then sem b r
                else sem c r)
          else failwith ("nonboolean guard")
      | Let(i,e1,e2) -> sem e2 (bind (r,i,sem e1 r))
