package lectures

object AdvancedPatternMatching extends App {

  val numbers = List(1)

  val description: Unit = numbers match {
    case head::Nil => println(s"the only element is $head")
    case _ =>
  }

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] =
      {
        if(person.age < 21) None
        else Some(person.name, person.age)
      }
    def unapply(age: Int): Option[String] = Some(if (age < 18) "minor" else "major")
  }

  val david = new Person("David", 30)

  val greeting = david match {
    case Person(a,b) => s"My name is $a and im $b"
  }

  val legalStatus = david.age match {
    case Person(status) => s"My legal status is $status"
  }

  println(greeting)
  println(legalStatus)

  object even {
    def unapply(arg: Int): Boolean = arg % 2 == 0
  }

  object singleDigit {
    def unapply(arg: Int): Boolean = arg > -10 && arg < 10
  }

  val n : Int = 48
  val mathProperty = n match {
    case singleDigit() => "single digit"
    case even() => "even"
    case _ => "No match"
  }

  println(mathProperty)

  //infix patterns
  case class Or[A,B](a: A, b: B)
  val either = Or(2, "two")
  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }
  println(humanDescription)

  // decomposing sequences
  val varargs = numbers match {
    case List(1, _*) => "starting with 1"
  }

  trait  MyList[+A]{
    def head: A = ???
    def tail: MyList[A] = ???
  }

  case object Empty extends  MyList[Nothing]
  case class Cons[+A](override val head: A, override val tail: MyList[A]) extends MyList[A]

  object  MyList {
    def unapplySeq[A] (list: MyList[A]): Option[Seq[A]] = {
      if(list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
    }
  }

  val myList : MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))

  val decomposed = myList match {
    case MyList(1, _*) => "starting with 1, 2 "
    case _ => "Something else"
  }

  println(decomposed)

  // custom return types for unapply
  // isEmpty: Boolean, get: Something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(arg: Person): Wrapper[String] = new Wrapper[String] {
       def isEmpty: Boolean = false
       def get: String = arg.name
    }
  }

  println(david match {
    case PersonWrapper(n) => s"This person's name is $n"
  })

}
