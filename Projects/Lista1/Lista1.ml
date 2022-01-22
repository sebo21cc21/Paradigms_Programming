(*Sebastian Bednarski*)

(*zadanie 1*)

let rec flatten1 xss =
  if xss = [] then failwith "empty list"
  else if List.length xss = 1 then List.hd xss
  else List.hd xss @ flatten1(List.tl xss);;

flatten1[[5;6];[1;2;3]] = [5;6;1;2;3];;
flatten1[[1;2;3];[4;5;6];[7;8;9]] = [1;2;3;4;5;6;1;2;3;7;8;9];;
flatten1[[];["Ala";"ma";"kota"]] = ["Ala";"ma";"kota"];;
flatten1[["Ala"];["ma"];["kota"];[]] = ["Ala";"ma";"kota"];;
flatten1[[];[]] = [];;
(*flatten1[];;*)


(*zadanie 2*)
let rec count(x, xs) = 
  if xs = [] then 0
  else if List.hd xs = x then count(x, List.tl xs) + 1
  else count(x, List.tl xs);;

count("a", []) = 0;;
count("a", ["a";"l";"a"]) = 2;;
count("a", ["a";"l";"a";"m";"a";"k";"o";"t";"a"]) = 4;;
count("1", ["1";"2";"3";"1"]) = 2;;


(*zadanie 3*)
let rec replicate(x,number) = 

  if number = 0 then []
  else if number < 0 then failwith "not positive number"
  else x::replicate(x,(number-1));;

replicate("la", 3) = ["la"; "la"; "la"];;
replicate("", 3) = ["";"";""];;
replicate("la",0) = [];;
replicate(1,5) = [1;1;1;1;1];;
(*replicate("la",-1);;*)


(*zadanie 4*)
let rec sqrList xs =
  if xs = [] then failwith "empty list"
  else if List.tl xs <> [] then (List.hd xs)*(List.hd xs)::(sqrList(List.tl xs))
  else List.tl xs @ ([(List.hd xs)*(List.hd xs)]);;

sqrList[1;2;3;4] = [1;4;9;16];;
sqrList[(-1); 7; (-7)] = [1;49;49];;
(*sqrList[];;*)


(*zadanie 5*)
let rec palindrome xs =
  if xs = [] then failwith "empty list"
  else xs = List.rev xs;;

palindrome["a";"l";"a"] = true;;
palindrome["A";"l";"a"] = false;;
palindrome["k";"o";"t"] = false;;
(*palindrome[];;*)


(*zadanie 6*)
let rec listLength xs=
  if xs = [] then 0
  else listLength(List.tl xs) + 1;;

listLength [1;2;3;4;5] = 5;;
listLength ["a";"l";"a"] = 3;;
(*listLength [] = 0;*)




