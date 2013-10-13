import scala.util.Random
import java.io.PrintWriter
import java.io.BufferedReader
import java.io.InputStreamReader
object Main {
  def main(args: Array[String]): Unit = {
    val in = new InputReader(System.in)
    System.err.println(Utility.measureTime {
      for (_ <- 1 to 1000000) {
        val a, b = in.next().toInt
      }
    } + "ms")
    //    val in = new InputReader(System.in)
    //    System.err.println(Utility.measureTime{
    //      for(_ <- 1 to 1000000)in.next().toInt
    //    }+"ms")
    //    val br = new BufferedReader(new InputStreamReader(System.in))
    //    System.err.println(Utility.measureTime{
    //      for(_ <- 1 to 1000000)br.readLine().toInt
    //    }+"ms")

    //SegmentTreeTest.Test()
    //PartialSumTest.Test()
    //Utility.makeTree(100000)
    //Utility.makePath(100000)
  }

}