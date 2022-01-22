(* Sebastian Bednarski *)

(* zadanie 2 *)

let rec f x = f x ;;


(* zadanie 3 *)

type 'a bt = Empty | Node of 'a * 'a bt * 'a bt;;

let t = Node(1,
             Node(2,
                  Empty,
                  Node(3,
                       Empty,
                       Empty)
                 ),
             Empty
            );;

let tt = Node(1,
              Node(2,
                   Node(4,
                        Empty,
                        Empty
                       ),
                   Empty
                  ),
              Node(3,
                   Node(5,
                        Empty,
                        Node(6,
                             Empty,
                             Empty
                            )
                       ),
                   Empty
                  )
             );;

let breadthBT tree=
  let rec helpFun queue=
    match queue with
        [] -> []
      | Empty::tail -> helpFun tail
      | Node(value,leftSubtree, rightSubtree) :: tail -> value :: helpFun(tail @ [leftSubtree;rightSubtree]) in 
    helpFun [tree];;

breadthBT t = [1; 2; 3];;
breadthBT tt = [1; 2; 3; 4; 5; 6];;
(* zadanie 4 a *)

let internalPath tree=
  let rec helpFun depth = function
      Empty -> 0
    | Node(_,leftSubtree, rightSubtree) -> depth + helpFun (depth+1) leftSubtree + helpFun (depth + 1) rightSubtree
  in helpFun 0 tree;;

internalPath t = 3;;
internalPath tt = 9;;

(* zadanie 4 b *)

let externalPath tree =
  let rec helpFun depth = function
      Empty -> depth
    | Node(_, leftSubtree, rightSubtree) -> helpFun(depth + 1) leftSubtree + helpFun (depth+1) rightSubtree
  in helpFun 0 tree;;

externalPath t = 9;;
externalPath tt = 21;;

(* zadanie 5 *)

type 'a graph = Graph of ('a-> 'a list);;

let g = Graph
          (function
              0-> [3]
            | 1 -> [0;2;4]
            | 2 -> [1]
            | 3 -> []
            | 4 -> [0;2]
            | n -> failwith ("Graph g: node "^string_of_int n^" doesnot exist")
          );;

let depthSearch(Graph succ) startNode=
  let rec search visited queue =
    match queue with
        [] -> []
      | h::t -> if List.mem h visited then search visited t
          else h::search (h::visited) (succ h@t)
  in search[] [startNode];;

depthSearch g 4 = [4; 0; 3; 2; 1];;
depthSearch g 0 = [0; 3];;
depthSearch g 1 = [1; 0; 3; 2; 4];;
depthSearch g 2 = [2; 1; 0; 3; 4];;
depthSearch g 3 = [3];;
