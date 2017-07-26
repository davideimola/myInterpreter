Printf.printf ("ESEMPIO: Fattoriale\n");;

let s1 = "Esame";;
let s2 = "Di";;
let s3 = "Linguaggi";;

Printf.printf ("\nSimple Estring\n");;
sem (Estring s1) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nLength of String s2\n");;
sem (Len(Estring s2)) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nConcat of the 3 strings\n");;
sem (Conc(Conc(Estring s1,Estring s2),Estring s3)) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nSubstring of String s3\n");;
sem (Subs(Estring s3,Eint 1,Eint 4)) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nSubstring of String s3 in 5,5\n");;
sem (Subs(Estring s3,Eint 5,Eint 5)) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nString equals s1 and s2\n");;
sem (Streq(Estring s1,Estring s2)) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nString equals s3 and Estring \"Linguaggi\"\n");;
sem (Streq(Estring s3,Estring "Linguaggi")) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nChar 5 in string s3 [Java String.charAt(int i)]\n");;
sem (Charat(Estring s3,Eint 5)) (emptyenv Unbound) (emptystore Undefined);;
Printf.printf ("\nSubs error\n");;
sem (Subs(Estring s3,Eint 31,Eint 33)) (emptyenv Unbound) (emptystore Undefined);;
(*sem (Subs(Estring s3,Eint 3,Eint 0)) (emptyenv Unbound) (emptystore Undefined);;*)
