package lectures.part2

import scala.annotation.tailrec

trait MySet[A] extends (A => Boolean) {

  def apply(elem : A): Boolean = contains(elem)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]

  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def forEach(e: A => Unit): Unit

  def -(elem: A):MySet[A]
  def --(anotherSet: MySet[A]): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A] //intersection
  def unary_! : MySet[A]
}


class EmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false
  override def +(elem: A): MySet[A] = new NonEmptySet[A](elem, new EmptySet)
  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  override def map[B](f: A => B): MySet[B] = new EmptySet[B]
  override def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  override def filter(predicate: A => Boolean): MySet[A] = this
  override def forEach(e: A => Unit): Unit = ()
  override def -(elem: A): MySet[A] = this
  override def --(anotherSet: MySet[A]): MySet[A] = this
  override def &(anotherSet: MySet[A]): MySet[A] = this
  override def unary_! : MySet[A] = new PropertyBasedSet[A](_=>true)
}

/*class AllInclusiveSet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = true
  override def +(elem: A): MySet[A] = this
  override def ++(anotherSet: MySet[A]): MySet[A] = this

  override def map[B](f: A => B): MySet[B] = ???
  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???
  override def filter(predicate: A => Boolean): MySet[A] = ???
  override def forEach(e: A => Unit): Unit = ???
  override def -(elem: A): MySet[A] = ???

  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  override def unary_! : MySet[A] = new EmptySet[A]
}*/

//all elements of type A which satisfy a property
class PropertyBasedSet[A](property: A=> Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)
  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == anotherSet(x))
  override def map[B](f: A => B): MySet[B] = politelyFail
  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def forEach(e: A => Unit): Unit = politelyFail
  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))
  override def -(elem: A): MySet[A] = filter(_!=elem)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really complicated problem =(")
}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = elem == head || tail.contains(elem)
  override def +(elem: A): MySet[A] = if (this contains elem) this else new NonEmptySet[A](elem, this)
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head
  override def map[B](f: A => B): MySet[B] = tail.map(f) + f(head)
  override def flatMap[B](f: A => MySet[B]): MySet[B] = tail.flatMap(f) ++ f(head)
  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail.filter(predicate)
    if(predicate(head)) filteredTail + head
    else filteredTail
  }
  override def forEach(f: A => Unit): Unit = {f(head);tail forEach f}
  override def -(elem: A): MySet[A] = if(head == elem) tail else tail - elem + head
  override def --(anotherSet: MySet[A]): MySet[A] = filter(x => !anotherSet.contains(x))
  override def &(anotherSet: MySet[A]): MySet[A] = filter(x => anotherSet.contains(x))
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => this.contains(x))
}

object MySet {
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(velSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if(velSeq.isEmpty) acc
      else buildSet(velSeq.tail, acc + velSeq.head)
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object RunningCode extends App {
  val s = MySet(1,2,3,4)
  val negative  = !s

  println(negative(2))
  println(negative(5))
  println{(negative + 5)(5)}
}