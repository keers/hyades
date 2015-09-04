import scala.io.StdIn
import util.control.Breaks._

/**
 * @see https://www.hackerrank.com/challenges/the-grid-search
 */
object TheGridSearch {

  def readMatrix(): Array[Array[Int]] = {
    val dim = StdIn.readLine().split(" ").map(_.toInt)
    val m = dim(0)
    val n = dim(1)
    val matrix = Array.ofDim[Int](m, n)

    (1 to m).foreach(r => matrix(r - 1) = StdIn.readLine().map(c => c.toInt - '0'.toInt).toArray)

    matrix
  }

  def contains(big: Array[Array[Int]], small: Array[Array[Int]]): Boolean = {
    for (i <- 0 until (big.length - small.length) + 1) {
      for (j <- 0 until (big(i).length - small(0).length) + 1) {

        breakable {
          for (m <- small.indices) {
            for (n <- small(m).indices) {
              if (big(i + m)(j + n) != small(m)(n)) {
                break()
              }
            }
          }
          return true
        }
      }
    }
    false
  }

  def main(args: Array[String]) {
    (1 to StdIn.readInt())
      .map(testCaseNum => {
      val big = readMatrix()
      val small = readMatrix()
      contains(big, small)
    }).foreach(i => println(if (i) "YES" else "NO"))
  }
}