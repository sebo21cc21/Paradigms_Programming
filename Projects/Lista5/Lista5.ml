(* Sebastian Bednarski *)

(* funkcje do zadania 1 i 2*)
type 'a llist = LNil | LCons of 'a * 'a llist Lazy.t;;

let rec lfrom k = LCons (k, lazy(lfrom (k+1)));;

let rec toLazyList = function
    [] -> LNil
  | x :: xs -> LCons (x, lazy (toLazyList xs));;

let rec ltake = function
    (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons (x, lazy xs)) -> x :: ltake (n-1, xs);;


(* zadanie 2 *)
let lfib =
  let rec fibHelper a b =
    LCons(a, lazy (fibHelper b (a+b)))
  in fibHelper 0 1;;

ltake(10, lfib) = [0; 1; 1; 2; 3; 5; 8; 13; 21; 34];;
ltake(7, lfib) = [0; 1; 1; 2; 3; 5; 8];;


(* zadanie 3 *)

type 'a lBT = LEmpty | LNode of 'a * (unit ->'a lBT) * (unit -> 'a lBT);;

(*podpunkt b*)
let rec lTree n =
  LNode(n, (function() -> lTree(2*n)), function () -> lTree(2*n+1));;

lTree 10;;
lTree 15;;

