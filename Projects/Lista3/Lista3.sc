//Sebastian Bednarski

//zadanie 2

//a)
def curry3[A,B,C,D](f: (A, B, C) => D) =
  (x:A) => (y:B) => (z:C) => f(x, y, z)

def curry3Lukier[A,B,C,D](function: (A, B, C) => D)(x:A)(y:B)(z:C): D = function(x, y, z)

//b)
def uncurry3[A,B,C,D](f: A => B => C => D) =
  (x:A, y:B, z:C) => f(x)(y)(z)
//zlukrowane
def uncurry3Lukier[A,B,C,D](f: A => B => C => D)(x:A, y:B, z:C): D = f(x)(y)(z)


//zadanie 3
def sumProd(xs:List[Int]):(Int, Int)=
  xs match{
    case h::t => {
      val(s,p) = sumProd(t)
      (h+s, h*p)
    }
    case Nil =>(0,1)
  }

def sumProd2(xs:List[Int]):(Int, Int) =
  xs match {
    case Nil => (0,1)
    case head::tail => xs.foldLeft(0,1) ((accum,head) => (accum._1 + head, accum._2 * head))
  }


sumProd2(List(1,2,3,4)) == sumProd(List(1,2,3,4))
sumProd2(List(3,7,8)) == (18,168)
sumProd2(List()) == (0,1)
