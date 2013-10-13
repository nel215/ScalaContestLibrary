import scala.util.Random
class PartialSum(arr:Array[Int]) {
  val n = arr.size
  val sum = Array.fill(n+1)(0)
  for((i,v) <- (0 until n).zip(arr)){
    sum(i+1) = sum(i)+v
  }
  def query(i:Int)={
    sum(i)
  }
  def query(l:Int,r:Int)={
    sum(r)-sum(l)
  }
}

object PartialSumTest{
    def Test() {
    val n = 128 * 128
    val arr = Array.fill(n)(0)
    val rng = Random
    for (i <- 0 until n) arr(i) = rng.nextInt()
    val ps = new PartialSum(arr)
    val t = 1000
    var time: Long = 0
    for (_ <- 0 until t) {
      var l = rng.nextInt(n)
      var r = rng.nextInt(n)
      if (r < l) { var tmp = l; l = r; r = tmp }
      r += 1
      var sum = 0
      for (i <- l until r)sum += arr(i)
      var q = 0
      time += Utility.measureTime {
        q = ps.query(l, r)
      }
      if (q!=sum) System.err.println("!")
    }
    System.err.println(time * 1.0 / t)
  }
}