import scala.util.Random
import java.io.PrintWriter
object Utility {
  def measureTime(func: => Unit): Long = {
    val start = System.currentTimeMillis()
    func
    return System.currentTimeMillis() - start
  }
  def makeTree(n: Int) = {
    val out = new PrintWriter(System.out)
    val rng = Random

    out.println(n)
    for (to <- 1 to n - 1) {
      val from = rng.nextInt(to)
      val cost = rng.nextInt(10000)
      out.println((from + 1) + " " + (to + 1) + " " + cost)
    }
    out.flush()
  }
  def makePath(n: Int) = {
    val out = new PrintWriter(System.out)
    val rng = Random

    out.println(n)
    for (to <- 1 to n - 1) {
      val from = to - 1
      val cost = rng.nextInt(10000)
      out.println((from + 1) + " " + (to + 1) + " " + cost)
    }
    out.flush()
  }
}