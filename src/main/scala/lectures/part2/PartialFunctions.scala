package lectures.part2

object PartialFunctions extends App {

  val aNormalFunction = (x: Int) => x+1 //Function1[Int,Int] === Int => Int

  val aFussyFunction = (x: Int) => // {1,2,3} => Int (Partial Function)
    x match {
      case 1 => 44
      case 2 => 988
      case 3 => 212
    }

  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 44
    case 2 => 988
    case 3 => 212
  }

  println(aPartialFunction(3))

  println(aPartialFunction.isDefinedAt(5))

  // lift
  val lifted = aPartialFunction.lift // Int => Option[Int]
  println(lifted(2))
  println(lifted(44))

  val pfChain = aPartialFunction.orElse[Int, Int] {
    case 45 => 67
  }

  println(pfChain(2))
  println(pfChain(45))

  // PF extends normal functions
  val aFunction: Int => Int = {
    case 1 => 99
  }

  // HOFs accept partial functions as well
  val aMappedList = List(1,2,3).map{
    case 1 => 42
    case 2 => 88
    case 3 => 1000
  }
  println(aMappedList)

  // !!!!PF can only have ONE parameter type!!!!


  /**
   * EXERCISES
   */

  val aManualFuzzy = new PartialFunction[Int, Int] {
    override def apply(v1: Int): Int = v1 match {
      case 1 => 42
      case 2 => 88
      case 3 => 1000
    }
    override def isDefinedAt(x: Int): Boolean = x==1||x==2||x==3
  }

  val bot: PartialFunction[String, String] = {
    case "hello" => "hello human"
    case "bye" => "see you later alligator"
  }

  scala.io.Source.stdin.getLines().foreach(l => println(bot(l)))
}
