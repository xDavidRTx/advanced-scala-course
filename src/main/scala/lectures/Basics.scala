package lectures

import scala.annotation.tailrec

object Basics extends App {

  @tailrec
  def factorial (n : Int, acc: Int = 1) : Int = {
    n match {
      case 0 => acc
      case _  => factorial(n -1, n*acc)
    }
  }

  //subtyping polymorphism
  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a : Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
   override def eat(a : Animal): Unit = println("crunch!")
  }

  val aCroc = new Crocodile

  // natural language
  aCroc eat aDog

  // anonymous classes
  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("roar!!")
  }

  // generics
  abstract class MyList[+A]

  //Singletons and companions
  object MyList

  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x+1

  List(1,2,3).map(anonymousIncrementer)
}
