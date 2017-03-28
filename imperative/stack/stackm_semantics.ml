(*
* Languages and Compilers - Languages (2016/17)
* Università di Verona
*
* Imola Davide - VR386238
* Slemer Andrea - VR386253
*)

module SemStack_Modificable: STACK_MODIFICABLE =
  struct
    type 'a stack = ('a SemStack.stack) ref
    exception Emptystack
    exception Fullstack
    exception Wrongaccess
    let emptystack (n, a) = ref(SemStack.emptystack(n, a))
    let length x = SemStack.length(!x)
    let push (a, p) = p := SemStack.push(a, !p)
    let pop x = x := SemStack.pop(!x)
    let top x = SemStack.top(!x)
    let empty x = SemStack.empty !x
    let rec clear x = if empty(x)
                      then ()
                      else (pop x; clear x)
    let rec faccess (x,n) = if n = 0
                            then SemStack.top(x)
                            else faccess(SemStack.pop(x), n-1)
    let access (x,n) = let nofpops = length(x) - 1 - n in
        if nofpops < 0
        then raise Wrongaccess
        else faccess(!x, nofpops)
  end
