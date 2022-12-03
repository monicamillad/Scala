import org.scalatest.flatspec.AnyFlatSpec

class MyListTest extends AnyFlatSpec {
  val listOfIntegers: MyList[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val listOfIntegersUnordered: MyList[Int] = Cons(8, Cons(3, Cons(2, Cons(1, Cons(7, Empty)))))
  val anotherListOfIntegers: MyList[Int] = Cons(4, Cons(5, Empty))
  val listOfStrings: MyList[String] = Cons("Hello", Cons("Scala", Empty))

  "map" should "double" in {
    assert(listOfIntegers.map(_ * 2).equals(Cons(2, Cons(4, Cons(6, Empty)))))
  }

  "flatmap" should "list + 1" in {
    assert(listOfIntegers.flatMap(elem => Cons(elem, Cons(elem + 1, Empty)))
      .equals(Cons(1, Cons(2, Cons(2, Cons(3,Cons(3,Cons(4,Empty))))))))
  }

  "++" should "" in {
    assert((listOfIntegers ++ anotherListOfIntegers)
      .equals(Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Empty)))))))
  }

  "filter" should "g 1" in {
    assert(listOfIntegers.filter(_ > 1)
      .equals(Cons(2, Cons(3,Empty))))
  }

  "filter" should "even" in {
    assert(listOfIntegers.filter(_ % 2==0)
      .equals(Cons(2, Empty)))
  }

  "add" should "6" in {
    assert(listOfIntegers.add(6)
      .equals(Cons(6,Cons(1, Cons(2, Cons(3, Empty))))))
  }

  "add end" should "6" in {
    assert(listOfIntegers.addEnd(6)
      .equals(Cons(1,Cons(2, Cons(3, Cons(6, Empty))))))
  }

  "zip" should "ints and strings" in {
    assert(anotherListOfIntegers.zipWith[String, String](listOfStrings, _ + "-" + _)
      .equals(Cons("4-Hello",Cons("5-Scala",Empty))))
  }

}
