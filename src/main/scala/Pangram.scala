import scala.io.StdIn

/**
 * @author keers
 * @see https://www.hackerrank.com/challenges/pangrams
 */
object Pangram {

  def main(args: Array[String]) {
    if (pangram(StdIn.readLine())) println("pangram") else println("not pangram")
  }

  def pangram(line: String): Boolean = {
    val letters: Array[Int] = new Array('z' - 'a' + 1)
    for (i <- letters.indices) {
      letters(i) = 0
    }
    line.filter(a => a != ' ').map(_.toLower).foreach(l => letters(l - 'a') += 1)
    !letters.exists(i => i < 1)
  }
}
