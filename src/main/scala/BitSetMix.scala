/**
 * Given unsigned integer 'x', write an algorithm that returns unsigned integer 'y' such that it has the same number of
 * bits set as 'x' and is not equal to 'x' and the distance |x-y| is minimized.
 *
 * Example:
 * x: 01
 * y: 10
 * Note that one bit is set and 'x' is not equal 'y'. You may assume that x is positive integer between zero and 2^32-2;
 * @see  http://www.careercup.com/question?id=5086215957118976
 * @author sergei.kirsanov
 */
object BitSetMix {

  def bestMatch(input: Long): Long = {
    if (input < 1 || input > Math.pow(2, 32).toLong - 2)
      throw new IllegalArgumentException

    def calculateMask(num: Long, mask: Int): Int = {
      val shifted = num >> 1
      if ((shifted & 1) != (num & 1))
        mask
      else
        calculateMask(num >> 1, mask << 1)
    }
    //Exchange the lowest order adjacent 1 and 0
    input ^ calculateMask(input, 3)
  }
}
