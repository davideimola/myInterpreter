Printf.printf("ESEMPI: Reflect\n");;

(* ESEMPIO 1 *)
Printf.printf("\n\n");;
Printf.printf("ESEMPIO 1\n");;
let s = "Sum(Eint 1,Sum(Eint 5,Eint 4))";;
let com = Reflect(Estring s);;

let (l,sigma) = allocate(emptystore(Undefined), Undefined);;
let rho = bind (emptyenv(Unbound), "result", Dloc l);;
let sigma1 = semc com rho sigma;;

#use "scripts/output.ml";;

(* ESEMPIO 2 *)
Printf.printf("\n\n");;
Printf.printf("ESEMPIO 2\n");;
let s = "Sum(Sum(Eint 10,Eint 1),Diff(Eint 2,Eint 1))";;
let com = Reflect(Estring s);;

let (l,sigma) = allocate(emptystore(Undefined), Undefined);;
let rho = bind (emptyenv(Unbound), "result", Dloc l);;
let sigma1 = semc com rho sigma;;

#use "scripts/output.ml";;

(* ESEMPIO 3 *)
Printf.printf("\n\n");;
Printf.printf("ESEMPIO 3\n");;
let s = "Subs(Estring CIAO,Sum(Eint 0,Eint 1),Sum(Eint 1,Eint 1))";;
let com = Reflect(Estring s);;

let (l,sigma) = allocate(emptystore(Undefined), Undefined);;
let rho = bind (emptyenv(Unbound), "result", Dloc l);;
let sigma1 = semc com rho sigma;;

#use "scripts/output.ml";;

(* ESEMPIO 4 *)
Printf.printf("\n\n");;
Printf.printf("ESEMPIO 4\n");;
let s = "Sum(Diff(Eint 5,Minus(Minus(Eint 1))),Len(Estring ciao))";;
let com = Reflect(Estring s);;

let (l,sigma) = allocate(emptystore(Undefined), Undefined);;
let rho = bind (emptyenv(Unbound), "result", Dloc l);;
let sigma1 = semc com rho sigma;;

#use "scripts/output.ml";;
