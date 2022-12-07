
object PascalTriangle {
  //    In Pascal's Triangle the number at the edge of the triangle are all 1,
  //    and each number inside the triangle is the sum of the two numbers above it.
  //    A sample Pascal's triangle would look like below.
  //
  //                1
  //              1   1
  //             1  2  1
  //            1  3 3  1
  //           1 4  6  4 1
  // write a program that computes the elements of Pascal's triangle, where input is the index of the element (Column, Row) and the output it the traingle element value
  def main(args: Array[String]): Unit = {
    assertResult(1, pascalTriangle(0, 0))
    assertResult(4, pascalTriangle(1, 4))
  }

  def assertResult(expected: Int, actual: Int): Unit = {
    assert(actual == expected, s"expected ${expected} but found ${actual}")
  }

  def pascalTriangle(col: Int, row: Int): Int = {
    getPascalTriangleRow(row)(col)
  }

  def getPascalTriangleRow(row: Int): List[Int] = {
    if (row == 0) List(1)
    else {
      val prevRow = getPascalTriangleRow(row - 1)
      val currentRow = for {
        i <- 0 to row
      } yield getElement(i - 1, prevRow) + getElement(i, prevRow)
      currentRow.toList
    }
  }

  def getElement(index: Int, list: List[Int]): Int = {
    if (index < 0 || index >= list.length) 0
    else list(index)
  }
}
