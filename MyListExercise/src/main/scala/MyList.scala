/**
 * Created by Daniel.
 */
abstract class MyList[+A] {

  /*
     head = first element of  the  list
     tail = remainder of the list
     isEmpty = is this list. empty
     add(int) => new list with this element added
     toString => a string representation of the list
   */

  def head: A
  def tail: MyList[A]
  def isEmpty: Boolean
  def add[B >: A](element: B): MyList[B]
  def addEnd[B >: A](element: B): MyList[B]
  def printElements: String
  // polymorphic call
  override def toString: String = "[" + printElements + "]"

  // higher-order functions
  def map[B](transformer: A => B): MyList[B]
  def flatMap[B](transformer: A => MyList[B]): MyList[B]
  def filter(predicate: A => Boolean): MyList[A]

  // concatenation
  def ++[B >: A](list: MyList[B]): MyList[B]

  // hofs
  def foreach(f: A => Unit): Unit
  def zipWith[B, C](list: MyList[B], zip:(A, B) => C): MyList[C]
}

case object Empty extends MyList[Nothing] {
  def head: Nothing = throw new NoSuchElementException
  def tail: MyList[Nothing] = throw new NoSuchElementException
  def isEmpty: Boolean = true

  def add[B >: Nothing](element: B): MyList[B] = {
    Cons(element, Empty)
  }

  def addEnd[B >: Nothing](element: B): MyList[B] = {
    add(element)
  }

  def printElements: String = ""

  def map[B](transformer: Nothing => B): MyList[B] = Empty
  def flatMap[B](transformer: Nothing => MyList[B]): MyList[B] = Empty
  def filter(predicate: Nothing => Boolean): MyList[Nothing] = Empty

  def ++[B >: Nothing](list: MyList[B]): MyList[B] = list

  def foreach(f: Nothing => Unit): Unit = ()

  def zipWith[B, C](list: MyList[B], zip: (Nothing, B) => C): MyList[C] = Empty
}

case class Cons[+A](h: A, t: MyList[A]) extends MyList[A] {
  def head: A = h
  def tail: MyList[A] = t
  def isEmpty: Boolean = false

  def add[B >: A](element: B): MyList[B] = {
    Cons(element, this)
  }

  def addEnd[B >: A](element: B): MyList[B] = {
    Cons(h, t.addEnd(element))
  }

  def printElements: String = {
    if(t.isEmpty) h + t.printElements
    else h + ", " + t.printElements
  }

  def filter(predicate: A => Boolean): MyList[A] = {
    if (t.isEmpty) {
      if (predicate(h)) Cons(h, Empty)
      else Empty
    }
    else {
      if (predicate(h)) Cons(h, t.filter(predicate))
      else t.filter(predicate)
    }
  }

  def map[B](transformer: A => B): MyList[B] = {
    Cons(transformer(h), t.map(transformer))
  }

  def ++[B >: A](list: MyList[B]): MyList[B] = {
    Cons(h, t ++ list)
  }

  def flatMap[B](transformer: A => MyList[B]): MyList[B] = {
    transformer(h) ++ t.flatMap(transformer)
  }

  def foreach(f: A => Unit): Unit = {
    f(h)
    t.foreach(f)
  }

  def zipWith[B, C](list: MyList[B], zip: (A, B) => C): MyList[C] = {
    if (t.isEmpty || list.tail.isEmpty) Cons(zip(h, list.head), Empty)
    else Cons(zip(h, list.head), t.zipWith(list.tail, zip))
  }

}


object ListTest extends App {

  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val cloneListOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val anotherListOfIntegers: MyList[Int] = Cons(4, Cons(5, Empty))
  val listOfStrings: MyList[String] = Cons("Hello", Cons("Scala", Empty))

  println(listOfIntegers.toString)
  println(listOfStrings.toString)

  println(listOfIntegers.map(_ * 2).toString)

  println(listOfIntegers.filter(_ % 2 == 0).toString)

  println((listOfIntegers ++ anotherListOfIntegers).toString)
  println(listOfIntegers.flatMap(elem => new Cons(elem, new Cons(elem + 1, Empty))).toString)

  println(cloneListOfIntegers == listOfIntegers)

  listOfIntegers.foreach(println)
  println(anotherListOfIntegers.zipWith[String, String](listOfStrings, _ + "-" + _))

  // for comprehensions
  val combinations = for {
    n <- listOfIntegers
    string <- listOfStrings
  } yield n + "-" + string
  println(combinations)
}