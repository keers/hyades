import scala.collection.mutable
import scala.io.StdIn

/**
 * https://www.hackerrank.com/challenges/bfsshortreach
 */
object BreadthFirstSearchShortestReach {

  def traverse(m: Array[Array[Int]], start: Int): Unit = {
    val queue = mutable.Queue((start, 0))

    while (queue.nonEmpty) {

      val (num, len) = queue.dequeue()

      for (j <- m.indices) {
        if (m(num)(j) == 1) {
          m(num)(j) = len + 6
          m(j)(num) = 0
          queue.enqueue((j, len + 6))
        }
      }
    }

    for (i <- m.indices; j <- m.indices) {
      if (m(i)(j) == 1)
        m(i)(j) = 0
    }


    for (j <- m.indices) {
      if (j != start) {

        var l = List[Int]()

        for (i <- m.indices) {
          if (m(i)(j) != 0)
            l = l :+ m(i)(j)
        }

        if (l.isEmpty) {
          print(-1)
        } else
          print(l.min)
        print(" ")
      }
    }
  }

  def main(args: Array[String]) {
    (1 to StdIn.readInt()).foreach(tc => {
      val nums = StdIn.readLine().split(" ").map(_.toInt)

      val N = nums(0) //vertices #
      val M = nums(1) //edges #

      val graph = Array.ofDim[Int](N, N)
      for (i <- 0 until N; j <- 0 until N) {
        graph(i)(j) = 0
      }

      for (i <- 1 to M) {
        val nums2 = StdIn.readLine().split(" ").map(_.toInt)
        val v0 = nums2(0) - 1
        val v1 = nums2(1) - 1

        graph(v0)(v1) = 1
        graph(v1)(v0) = 1

      }
      val start = StdIn.readInt() - 1

      traverse(graph, start)
      println()
    })
  }
}