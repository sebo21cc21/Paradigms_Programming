//Sebastian Bednarski

//zadanie 1

def lrepeat[A](k:Int)( lxs:LazyList[A]):LazyList[A]= {
  def helper(rest: Int, llist: LazyList[A]):LazyList[A] =
    (rest, llist) match
      case (_, LazyList()) => LazyList()
      case (0, _ #::tail)=> helper(k, tail)
      case(_, head #:: _) => head#:: helper(rest-1, llist)

  helper(k, lxs)
}
lrepeat(3)(LazyList('a', 'b', 'c', 'd')).toList == List('a', 'a', 'a', 'b', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd')
lrepeat(3)(LazyList.from(1)).take(15).toList == List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5)
lrepeat(3)(LazyList()).take(15).toList == List()
lrepeat(3)(LazyList(1,2,3)).toList == LazyList(1, 1, 1, 2, 2, 2, 3, 3, 3)
lrepeat(0)(LazyList(1,2,3)).toList == LazyList()
lrepeat(5)(LazyList()).force == LazyList()

//zadanie 2

val lfibs =
  def fibHelper(a:Int, b:Int):LazyList[Int]=
    a #:: fibHelper(b, a + b)
  fibHelper(0,1)


lfibs.take(10).toList == LazyList(0,1,1,2,3,5,8,13,21,34)
lfibs.take(6).toList == LazyList(0,1,1,2,3,5)

//zadanie 3

sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]

//b
def lTree(n:Int):lBT[Int]=
  LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))

lTree(10)
//LNode(10,repl$.rs$line$12$<function>,repl$.rs$line$12$<function>)
lTree(15)
//LNode(15,repl$.rs$line$12$<function>,repl$.rs$line$12$<function>)
