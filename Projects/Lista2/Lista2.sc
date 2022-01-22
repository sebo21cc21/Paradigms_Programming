import scala.annotation.tailrec

//Sebastian Bednarski

//task 2
//Liczby Fibbonacciego

//a.1
def fib(n:Int) : Int =
  n match
  {
    case 0 => 0
    case 1 => 1
    case _ => if!(n <= 0) then fib(n-2) + fib(n-1)
    else throw new Exception ("n less than zero")
  }
fib(0)==0
fib(1)==1
fib(2)==1
fib(3)==2
fib(4)==3
fib(5)==5
//fibfunct(-5)

//a.2
val fibFun: Int => BigInt = (n:Int) =>
  if (n<0) throw new Exception ("n less than zero")
  else  if(n==0) 0
  else if (n==1) 1
  else fibFun(n-2) + fibFun(n-1)

fibFun(0)==0
fibFun(1)==1
fibFun(2)==1
fibFun(3)==2
fibFun(4)==3
fibFun(5)==5
//fibFun(-5)

//b.1
def fibTail(n: Int): Int = {

  @tailrec
  def fibIter(n: Int, accum1: Int, accum2: Int): Int =
    n match
    {
      case 0 => accum1
      case 1 => accum2
      case _ => fibIter(n-1, accum2, accum1+accum2)
    }
  fibIter(n, 0, 1)
}

fibTail(0)==0
fibTail(1)==1
fibTail(2)==1
fibTail(3)==2
fibTail(4)==3
fibTail(5)==5


//b.2
val fibTailFun: Int => BigInt = (n:Int) => {

  @tailrec
  def fibIter(n: Int, accum1: BigInt, accum2: BigInt): BigInt =
    n match
    {
      case 0 => accum1
      case 1 => accum2
      case _ => fibIter(n-1, accum2, accum1+accum2)
    }
  fibIter(n, 0, 1)
}

fibTailFun(0)==0
fibTailFun(1)==1
fibTailFun(2)==1
fibTailFun(3)==2
fibTailFun(4)==3
fibTailFun(5)==5


//sprawdzenie predkosci
fib(42)==267914296 //2-nie tak szybko
fibFun(42)==267914296 //3-najdluzej
fibTail(42)==267914296//1-blyskawicznie
fibTailFun(42)==267914296 //1-blyskawicznie

//zadanie 3

def root3check(a:Double):Double=
  a match
  {
    case x if x>1 => a/3
    case x if x<=1 => a
  }

def root3(a: Double): Double =
{
  @tailrec
  def root3Iter(x:Double, a:Double, e:Double, accum:Double): Double =
    (x,a,e) match
    {
      case (x,a,e) if (x*x*x-a).abs <= (e*a).abs => x
      case _ => root3Iter(accum, a, e, x+((a/(x*x))-x)/3)
    }
  root3Iter(root3check(a), a, 0.000000000000001, root3check(a))
}

root3(1.0)
root3(216.0)
root3(27.1)
root3(8.0)
root3(-8.0)

//zadanie 3b


val root3checkFun = (a:Double) => {
  a match {
    case x if (x > 1) => a / 3
    case x if (x <= 1) => a
  }
}
val root3Fun :Double => Double = (a: Double) =>
{
  @tailrec
  def root3IterFun(x:Double, a:Double, e:Double, accum:Double): Double =
    (x,a,e) match
    {
      case (x,a,e) if ((x*x*x-a).abs <= (e*a).abs) => x
      case _ => root3IterFun(accum, a, e, x+(((a/(x*x))-x))/3)

    }
  root3IterFun(root3checkFun(a), a, 0.000000000000001, root3checkFun(a))
}

root3Fun(1.0)
root3Fun(216.0)
root3Fun(27.1)
root3Fun(8.0)
root3Fun(-8.0)

//zadanie 4

//a
val x = (-2, -1, 0, 1, 2)
val (_,_,a,_,_) = x

a==0

//b
val y = (List(1,2), List(0,1))
val (List(_,_), List(b,_)) = y

b==0

//zadanie 5

def initSegment [A](xs: List[A], ys: List[A]): Boolean =
  if(xs==ys) true
  else if(xs == Nil) true
  else
  {
    val h1::t1 = xs
    val h2::t2 = ys

    h1==h2
  }

initSegment(List(1, 2), List(1, 8, 8))
initSegment(List(1, 2, 3), List(4, 5, 6))
initSegment(List(), List(1, 2, 3))
initSegment(List(1, 2, 3), List(1,2,3))
initSegment(List("a", "b", "c"), List("a", "b", "c"))
initSegment(List("a", "b", "c"), List("c", "b", "a"))

//zadanie 6
def replaceNth [A](xs: List[A], number: Int, x: A): List[A] =
  if (xs==Nil) throw new Exception ("empty list")
  else  if(number==0)
  {
    val h1 :: t1 = xs
    x :: t1
  }
  else if (number>0)
  {
    val h1 :: t1 = xs
    h1 :: replaceNth(t1, number-1, x)
  }
  else throw new Exception ("error, n less than zero")


replaceNth(List('o','l','a', 'm', 'a', 'k', 'o', 't', 'a'), 1, 's')
replaceNth(List('a', 'b', 'c', 'd'), 0, 'z')
replaceNth(List('a', 'b', 'c', 'd'), 1, 'z')
replaceNth(List('a', 'b', 'c', 'd'), 2, 'z')
replaceNth(List('a', 'b', 'c', 'd'), 3, 'z')
replaceNth(List('a', 'b', 'c', 'd'), 0, "" )
replaceNth(List(1,2,3,4,5), 3, 123)
//replaceNth(List(1,2,3,4,5), 5, "x")
//replaceNth(List(1,2,3,4,5), -1, "x")
//replaceNth(List(), 1, 'x')
