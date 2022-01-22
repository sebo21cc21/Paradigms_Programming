//Sebastian Bednarski

//zadanie 2
//a
def swap (tab:Array[Int], i:Int, j:Int):Unit= {
  val aux = tab(i)
  tab(i) = tab(j)
  tab(j) = aux
}

def choose_pivot(tab:Array[Int])(m:Int)(n:Int):Int= tab((m+n)/2)

//b
def partition(tab: Array[Int])(l: Int)(r: Int):(Int, Int) = {
  var i = l; var j = r; val pivot = choose_pivot(tab)(l)(r)
  while (i <= j) {
    while (tab(i) < pivot) i += 1
    while (pivot < tab(j)) j -= 1
    if (i <= j)
      swap(tab, i, j); i += 1; j -= 1
  }
  (i, j)
}

//c
def quick(tab: Array[Int])(l: Int)(r: Int):Unit=
  if (l < r) {
    val (i, j) = partition(tab)(l)(r)
    if (j - l < r - i) {
      quick(tab)(l)(j)
      quick(tab)(i)(r)
    }
    else {
      quick(tab)(i)(r)
      quick(tab)(l)(j)
    }
  }
  else ()

//d
def quicksort(tab: Array[Int]):Unit = quick(tab)(0)(tab.length - 1)

val array1 = Array(4, 3, 6, 1, 4, 2, 5, 6, 7)
quicksort(array1)
array1 sameElements Array(1, 2, 3, 4, 4, 5, 6, 6, 7)

val array2 = Array(1, 1, 1, 1, 2, 1, 1, 1)
quicksort(array2)
array2 sameElements Array(1, 1, 1, 1, 1, 1, 1, 2)

val array3 = Array(5, 4, 3, 3, 2, 1)
quicksort(array3)
array3 sameElements Array(1, 2, 3, 3, 4, 5)