//Sebastian Bednarski
//zadanie 1
class Time(private var actualHour:Int = 0):
  if(actualHour < 0 || actualHour > 23) actualHour = 0;

  def hour:Int = actualHour //dereferencja, akcesor, getter

  def hour_=(newHour: Int) = //przypisanie, mutator, setter
    if(newHour < 0 || newHour > 23) actualHour = 0;
    else actualHour = newHour

//zadanie 3
class Vehicle(private val producer: String, private val model: String, private val date: Int, private var registration: String = ""):
  def this(producent: String, model: String, registration: String) =
    this(producent, model, -1, registration)
  def this(producent: String, model: String) =
    this(producent, model, -1, "")

object Lista9 {
  def main(args: Array[String]) : Unit = {
    //testy zadanie 1
    val time = new Time(12)
    time.hour_=(-1)
    println(time.hour) //output 0
    time.hour_=(5)
    println(time.hour) //output 5
    time.hour = 25
    println(time.hour) //output 0

    //testy zadanie 3
    //odczyt pól tylko dla producenta
    val opel = new Vehicle("Opel", "Vectra", 2015, "WND82424")
    val audi = new Vehicle("Audi", "a4 b8", 2012)
    val bmw = new Vehicle("BMW", "e46", "W3SEBUS")
    val honda = new Vehicle("Honda", "Civic")
  }
}
