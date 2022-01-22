(*Sebastian Bednarski*)

(*zadanie 2*)

let rec fib n =
  if n<0 then failwith ("n less than zero")
  else if n==0 then 0
  else if n==1 then 1
  else fib(n-2) + fib(n-1);;

fib(0) = 0;;
fib(1) = 1;;
fib(2) = 1;;
fib(3) = 2;;
fib(4) = 3;;
fib(5) = 5;;
(*fib(-5);;*)

let rec fibTail n =
  let rec fibIter(n, accum1, accum2) =
    match n with
      |0 -> accum1
      |1-> accum2
      |_-> fibIter(n-1, accum2, accum1+accum2)
  in fibIter(n, 0, 1);;

fibTail(0) = 0;;
fibTail(1) = 1;;
fibTail(2) = 1;;
fibTail(3) = 2;;
fibTail(4) = 3;;
fibTail(5) = 5;;

(*test szybkosci*)
fib(42);; (*dlugo*)
fibTail(42);; (*blyskawicznie*)

(*zadanie 3*)

let rec root3check a=
  if (a > 1.0) then a/.3.0
  else a;;

let rec root3Bool (x,a,e)=
  abs_float (x*.x*.x-.a) <= abs_float (e*.a);;


let rec root3 a=
  let rec root3Iter(x,a,e,accum)=
    if root3Bool(x,a,e) then x
    else root3Iter(accum, a,e, x+.(((a/.(x*.x))-.x))/.3.0)
  in root3Iter(root3check(a), a, 0.000000000000001, root3check(a));;

root3(1.0) = 1.;;
root3(216.0) = 6.;;
root3(27.1) = 3.00369914061521;;
root3(8.0) = 2.;;
root3(-8.0) = (-2.);;

(*zadanie 4*)

(* a *)
let x = (-2, -1, 0, 1, 2);;
let (_,_,a,_,_) = x;;

a=0;;

(* b *)

let y = ([1,2],[0,1]);;
let ([_,_], [b,_]) = y;;

b=0;;

(*zadanie 5*)

let rec initSegment(xs, ys) =
  match (xs, ys) with
    	| ([],_) -> true
    	| (_,[]) -> false
    	| _ -> if List.hd xs = List.hd ys then initSegment(List.tl xs, List.tl ys) 
        else false;;



initSegment([1;2], [1;8;8]);;
initSegment([1;2;3], [4;5;6]);;
initSegment([],[1;2;3]);;
initSegment([1;2;3],[1;2;3]);;
initSegment(["a";"b";"c"],["a";"b";"c"]);;
initSegment(["a";"b";"c"],["c";"b";"a"]);;


(*zadanie 6*)

let rec replaceNth(xs, n, x) = 
  match (xs, n) with
      ([], _) -> failwith ("empty list")
    |(h1 :: t1, 0) -> x :: t1
    |(h1 :: t1, _) -> h1 :: replaceNth(t1, n-1, x);;





replaceNth(['o';'l';'a'],0,'s');;
replaceNth(['a';'b';'c';'d'],0,'z');;
replaceNth(['a';'b';'c';'d'],1,'z');;
replaceNth(['a';'b';'c';'d'],2,'z');;
replaceNth(['a';'b';'c';'d'],3,'z');;
(*replaceNth([1;2;3;4;5],5,2);;
  replaceNth([1;2;3;4;5],-1,3);;
  replaceNth([],1, 2);;*)

