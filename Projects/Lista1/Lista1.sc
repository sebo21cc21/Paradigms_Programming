// Sebastian Bednarski

//zadanie 1
def flatten1[A] (xss: List[List[A]]) : List[A] =
  if(xss == Nil) throw new Exception("empty list")
  else if(xss.length == 1) then xss.head
  else xss.head ::: flatten1(xss.tail)

flatten1(List(List(5,6), List(1,2,3))) == List(5,6,1,2,3)
flatten1(List(List(1,2,3), List(4,5,6), List(7,8,9))) == List(1,2,3,4,5,6,7,8,9)
flatten1(List(List(), List("Ala","ma","kota"))) == List("Ala", "ma", "kota")
flatten1(List(List("Ala","ma","kota"), List())) == List("Ala", "ma", "kota")
flatten1(List(List(),List())) == List()
//flatten1(List())

//zadanie 2
def count[A](x: A, xs: List[A]) : Int =
  if(xs == List()) then 0
  else if(xs.head == x) then count(x, xs.tail) + 1
  else count(x, xs.tail)

count("a", List()) == 0
count("a", List("a", "l", "a")) == 2
count("a", List("a", "l", "a", "m", "a", "k", "o", "t", "a")) == 4
count(1, List(1,2,3,1)) == 2

//zadanie 3
def replicate[A](x:A,n:Int) : List[A] =
  if n == 0 then List()
  else if n < 0 then throw new Exception("not positive number")
  else x::replicate(x,(n-1))

replicate("la", 3) == List("la", "la", "la")
replicate("", 3) == List("", "", "")
replicate("la", 0) == List()
replicate(1, 5) == List(1, 1, 1, 1, 1)
//replicate("la",-1)

//zadanie 4
def sqrList (xs:List[Int]) : List[Int] =
  if(xs == Nil) throw new Exception ("empty list")
  else if (xs.length == 1) List(xs.head*xs.head)
  else List(xs.head*xs.head):::sqrList(xs.tail)

//metoda
val sqrListmethod = sqrList

sqrList(List(1,2,3,4)) == List(1, 4, 9, 16)
sqrList(List(-1, 7, -7)) == List(1, 49, 49)
//sqrList(List())
sqrListmethod(List(1,2,3,4)) == List(1, 4, 9, 16)
sqrListmethod(List(-1, 7, -7)) == List(1, 49, 49)
//sqrListmethod(List())
//zadanie 5
def palindrome[A] (xs: List[A]): Boolean =
  if(xs == List()) then throw new Exception("empty list")
  else xs == xs.reverse

palindrome(List("a", "l", "a")) == true
palindrome(List("A", "l", "a")) == false
palindrome(List("k", "o", "t")) == false
//palindrome(List())

//zadanie 6
def listLength[A](xs: List[A]): Int =
  if(xs == Nil) then 0
  else listLength(xs.tail) + 1

listLength(List(1, 2, 3, 4, 5)) == 5
listLength(List("a", "l", "a")) == 3
listLength(List()) == 0
