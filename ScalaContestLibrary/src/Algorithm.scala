import scala.annotation.tailrec

object Algorithm {
  @tailrec
  def lower_bound(arr: Array[Int], first: Int, last: Int, v: Int): Int = {
    val count = last - first
    if (count > 0) {
      val step = count / 2
      val it = first + step
      if (arr(it) < v) lower_bound(arr, 1 + it, last, v)
      else lower_bound(arr, first, first + step, v)
    } else return first
  }
}
