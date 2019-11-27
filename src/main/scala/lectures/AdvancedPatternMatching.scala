package lectures

object AdvancedPatternMatching extends App {

  val numbers = List(1)

  val description = numbers match {
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
}
