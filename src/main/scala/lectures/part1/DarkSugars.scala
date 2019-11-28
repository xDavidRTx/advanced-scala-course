package lectures.part1

object DarkSugars extends App {

 def singleArg(arg: Int): String = s"$arg"

 val foo =  singleArg {
    //complex code here
    4
  }

  //single abstract method
  trait Action {
    def ap(x:Int):Int
  }

  val aFunkyInstance: Action = (x: Int) => x+1

  val aThread = new Thread(new Runnable {
    override def run(): Unit = println("Running stuffs")
  })

  val aSweeterThread = new Thread(( )=> println("Running sweeter =)"))

  abstract class AnAbstract {
    def implemented: Int = 99
    def unimplemented(a : Int): Unit
  }

  // No implicits here only syntax sugar
  val anAbstractInstance : AnAbstract = (b: Int) => println("implementing :p")

  val prependedList = 2 :: List(3,4) //head::Tail

  1::2::3::List(4,5)

  class MyStream[T] {
    def -->: (value: T): MyStream[T] = this //we need the : to do de associative magic
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  //multi-word method naming
  class Person(name :String) {
    def `full name`(name : String):Unit = println(s"Full name: $name")
  }

  val David = new Person("David")
  David `full name` "David Pereira"

  class Composite[A,B]

  val composite: Int Composite String = ???

  class-->[A,B]

  val towards: Int --> String = ???

 class Mutable {
   private var internal: Int = 0
   def member:Int = internal // getter
   def member_=(value:Int):Unit = internal = value // setter
 }

  val aMutable = new Mutable
  aMutable.member = 44
}
