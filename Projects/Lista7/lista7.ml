(*Sebastian Bednarski*)

module type QUEUE_FUN =
sig
  type 'a t
  exception Empty of string
  val empty: unit -> 'a t
  val enqueue: 'a * 'a t -> 'a t
  val dequeue: 'a t -> 'a t 
  val first: 'a t -> 'a
  val isEmpty: 'a t -> bool
end;;

(*zadanie 1 a*)


module QueueList : QUEUE_FUN =
struct
  type 'a t = 'a list

  exception Empty of string

  let empty() = []

  let enqueue (elem, q) =
    q @ [elem]

  let dequeue q = 
    match q with
        h :: t -> t
      | [] -> []

  let first q= 
    match q with
        h :: t -> h
      | [] -> raise (Empty "first elem - m QueueList")

  let isEmpty q = q = []

end;;

let q = QueueList.empty();;
let e1 = 1;;
let e2 = 2;;

QueueList.isEmpty(QueueList.enqueue(e1, q)) = false;;
QueueList.isEmpty(q) = true;;
QueueList.dequeue(QueueList.enqueue(e1, QueueList.enqueue(e2, q)))= QueueList.enqueue(e1, q);;
QueueList.dequeue(QueueList.enqueue(e1, q)) = q;;
QueueList.first(QueueList.enqueue(e1, q)) = e1;;
(* excception: QueueList.first (QueueList.empty());;*)

(*zadanie 1 b*)


module QueueList2 : QUEUE_FUN =
struct
  type 'a t = 'a list * 'a list

  exception Empty of string

  let empty() = ([],[])

  let enqueue (elem, q) =
    match q with
        ([], []) -> ([elem], [])
      | (xs, xss) -> (xs, elem :: xss)

  let dequeue (q) =
    match q with
        ([], []) -> ([],[])
      | (_ :: [], xss) -> (List.rev xss, [])
      | (_ :: t, xss) -> (t, xss)
      | ([], xss) -> raise (Empty "dequeue - m QueueList2")

  let first (q) =
    match q with
      | (h :: _, _) -> h
      | ([], _) -> raise (Empty "first elem - m QueueList2")

  let isEmpty q = q = ([],[])

end;;


let q = QueueList2.empty();;
let e1 = 1;;
let e2 = 2;;

QueueList2.isEmpty(QueueList2.enqueue(e1,q)) = false;;
QueueList2.isEmpty(q) = true;;
QueueList2.dequeue(QueueList2.enqueue(e1, QueueList2.enqueue(e2, q)))= QueueList2.enqueue(e1, q);;
QueueList2.dequeue(QueueList2.enqueue(e1, q)) = q;;
QueueList2.dequeue(q) = q;;
QueueList2.first(QueueList2.enqueue(e1, q)) = e1;;
QueueList2.first(QueueList2.enqueue(e1, QueueList2.enqueue(e2,q))) = QueueList2.first(QueueList2.enqueue(e2,q));;
(* excception: QueueList2.first (QueueList2.empty());;*)

