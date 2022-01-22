//Sebastian Bednarski

class UnderflowException (msg:String) extends Exception(msg)

//kolejka do zadania 3 FIFO- first in first out
class MyQueue[+T](private val queue: (List[T], List[T]) = (Nil, Nil)): //konstruktor glowny prywatny
  //dołożenie do kolejki elementu
  def enqueue[S >: T] (elem: S)  = queue match
    case (Nil, _) => new MyQueue[S](List(elem), Nil)
    case (_, _) => new MyQueue[S](queue._1, elem::queue._2)

  //wyjecie z kolejki elementu z front
  def dequeue : MyQueue[T] = queue match
    case (Nil, _) => new MyQueue[T](Nil, Nil)
    case (h::t, elem) => if(t == Nil) new MyQueue[T](elem.reverse, Nil)
                         else new MyQueue[T](t, elem)


  //pierwszy element
  def first : T = queue._1 match
    case Nil => throw new UnderflowException("class MyQueue: first")
    case h::_ => h


  //czy pierwsza lista jest pusta
  def isEmpty: Boolean = queue._1 == Nil


object MyQueue { //obiekt towarzyszący
  def apply[T](xs: T*) = new MyQueue[T](xs.toList, Nil)

  def empty[T] = new MyQueue[T](Nil, Nil)
}


object Lista10{
  def main (args: Array[String]): Unit = {
    val queue = MyQueue.empty
    val queue1 = MyQueue()
    val queue2 = MyQueue('a', 'b', 'c')
    val queue3 = new MyQueue

    println(queue.isEmpty) //output true
    println(queue.enqueue(1).enqueue(2).dequeue.first) //output 2
    println(queue1.isEmpty) //output ture
    println(queue1.enqueue("first").enqueue("second").dequeue.first) //output second
    println(queue2.dequeue.first) //output b
    println(queue3.enqueue(1).enqueue(2).dequeue.first) //output 2
  }
}