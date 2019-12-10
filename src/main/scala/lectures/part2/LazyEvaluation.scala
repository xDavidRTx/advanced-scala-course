package lectures.part2

object LazyEvaluation extends App {
  //lazy val x: Int = throw new RuntimeException
  lazy val x: Int = {
    println("hello")
    42
  }
  println(x)
  println(x)

  def sideEffectCondition: Boolean = {
    println("Boooo")
    true
  }
  def simpleCondition: Boolean = false

  lazy val lazyCondition = sideEffectCondition
  println(if(simpleCondition && lazyCondition) "yes" else "no") // sideEffect is not called!!!

  // in conjunction with call by name
  def byNameMethod(n: => Int): Int = n + n + n + 1
  def retrieveMagicValue = {
    // side effect or long computation
    println("waiting")
    Thread.sleep(1000)
    42
  }
  println(byNameMethod((retrieveMagicValue)))

  // use lazy vals
  // in conjunction with call by name
  def byNameMethod2(n: => Int): Int =  {
    lazy val t = n
    t + t + t + 1
  }

  println(byNameMethod2((retrieveMagicValue)))

  //filter with lazy vals
  def lessThan30(i: Int): Boolean = {
    println(s"$i is less than 30")
    i < 30
  }

  def graterThan20(i: Int): Boolean = {
    println(s"$i is grater than 20")
    i > 20
  }

  val numbers = List(1 , 25 , 40, 5 , 23)
  val lt30 = numbers.filter(lessThan30)
  val gt20 = lt30.filter(graterThan20)
  println(gt20)

  val lt30lazy = numbers.withFilter(lessThan30)
  val gt20lazy = lt30lazy.withFilter(graterThan20)

  println(gt20lazy)
  gt20lazy.foreach(println) // they are checked only when needed

  // for-comp use withFilter with guards
  for {
    a <- List(1,2,3) if a % 2 == 0
  } yield a + 1
  List(1,2,3).withFilter(_%2 == 0).map(_ + 1)

}
