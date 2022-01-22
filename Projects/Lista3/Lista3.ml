(* Sebastian Bednarski*)

(* zadanie 2 *)

let curry3zlukrowane f x y z = f (x, y, z);;
let curry3 = function f -> function x -> function y -> function z -> f (x, y, z);;

let uncurry3zlukrowane f (x, y, z) = f x y z;;
let uncurry3 = function f -> function (x, y, z) -> f x y z;; 

(* zadanie 3 *)
let rec sumProd xs =
  match xs with
      h::t -> let(s,p)=sumProd t
        in (h+s, h*p)
    | [] -> (0,1);;

let rec sumProd2 xs = 
  List.fold_left (
    fun s h -> match s with 
        (s,p) -> (s+h, p*h)
  ) (0,1) xs;;

sumProd2[1; 2; 3; 4] = sumProd[1; 2; 3; 4];;
sumProd2[3; 7; 8] = (18, 168);;
sumProd2[] = (0,1);;
