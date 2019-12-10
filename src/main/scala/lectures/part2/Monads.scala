package lectures.part2

object Monads extends App {

  // hand made Try monad
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }

  object Attempt{
    def apply[A](a:  => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] =
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }


  class Lazy[+A](value: => A) {
    // call by need
    private lazy val internalValue = value
    def use: A = internalValue
    def flatMap[B](f: (=>A) => Lazy[B]): Lazy[B] = f(internalValue)
  }

  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value) // unit
  }

  val lazyInst = Lazy {
    println("LAZYYYYY")
    42
  }

  val flatMappedInstance = lazyInst.flatMap(x => Lazy{10 * x})
  val flatMappedInstance2 = lazyInst.flatMap(x => Lazy{10 * x})

  flatMappedInstance.use
  flatMappedInstance2.use
}
