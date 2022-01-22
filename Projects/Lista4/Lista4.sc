//Sebastian Bednarski

//zadanie 3

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem:A, left:BT[A], right:BT[A]) extends BT[A]

val t = Node(1,
            Node(2,
                Empty,
                Node(3,
                    Empty,
                    Empty
                    )
                ),
            Empty
            )
/***
            1
         2    -
      -   3
         -  -
 ***/
val tt = Node(1,
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
              )

/***
            1
         2    3
      4   _  5  _
     _ _   _  6
             _ _
***/

def breadthBT[A](tt: BT[A]) = {
  def helpFun[A](queue: List[BT[A]]): List[A] = queue match
    case Nil => Nil
    case Empty :: tail => helpFun(tail)
    case Node(value, leftSubtree, rightSubtree) :: tail => value :: helpFun(tail ::: List(leftSubtree, rightSubtree))

  helpFun(List(tt))
}
breadthBT(t) == List(1, 2, 3)
breadthBT(tt) == List(1, 2, 3, 4, 5, 6)

//zadanie 4a

def internalPath[A](tt: BT[A]) = {
  def helpFun(node: BT[A], depth: Int): Int = node match
    case Empty => 0
    case Node(_, leftSubtree, rightSubtree) => depth + helpFun(leftSubtree, depth + 1) + helpFun(rightSubtree, depth + 1)

  helpFun(tt, 0)
}
internalPath(t) == 3
internalPath(tt) == 9

//zadanie 4b

def externalPath[A] (tt: BT[A]) = {
  def helpFun(node: BT[A], depth: Int): Int = node match
    case Empty => depth
    case Node(_, leftSubtree, rightSubtree) => helpFun(leftSubtree, depth + 1) + helpFun(rightSubtree, depth + 1)

  helpFun(tt, 0)
}
externalPath(t) == 9
externalPath(tt) == 21

//zadanie 5

sealed trait Graphs[A]
case class Graph[A](succ: A=>List[A]) extends Graphs[A]


val g = Graph((i: Int) => i match
  case 0 => List(3)
  case 1 => List(0,2,4)
  case 2 => List(1)
  case 3 => Nil
  case 4 => List(0,2)
  case n => throw new Exception("Graph g: node " + n
    + " doesn't exist")
)

def depthSearch[A](g: Graph[A])(startNode: A) = {
  def search(visited: List[A])(queue: List[A]): List[A] = queue match
    case Nil => Nil
    case h :: t =>
      if (visited contains h) search(visited)(t)
      else h :: search(h :: visited)((g succ h) ++ t)

  search(Nil)(List(startNode))
}

depthSearch(g)(4) == List(4, 0, 3, 2, 1)
depthSearch(g)(0) == List(0, 3)
depthSearch(g)(1) == List(1, 0, 3, 2, 4)
depthSearch(g)(2) == List(2, 1, 0, 3, 4)
depthSearch(g)(3) == List(3)