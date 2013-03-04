import scala.util.Random
class SegmentTree(arr: Array[Int]) {
  val INF = 1 << 28
  var n = 1
  while (n < arr.size) n = n << 1
  val data = Array.fill(2 * n - 1)(INF)
  val index = Array.fill(2 * n - 1)(0)
  for (i <- 0 until arr.size) {
    data(n - 1 + i) = arr(i)
    index(n - 1 + i) = i
  }
  for (i <- (0 until n - 1).reverse) {
    data(i) = Math.min(data(2 * i + 1), data(2 * i + 2))
    index(i) = if (data(i) == data(2 * i + 1)) index(2 * i + 1) else index(2 * i + 2)
  }

  def query(a: Int, b: Int, k: Int = 0, l: Int = 0, r: Int = n): (Int, Int) = {
    if (b <= l || r <= a) return (Int.MaxValue, -1)
    if (a <= l && r <= b) return (data(k), index(k))
    val mid = (l + r) >> 1
    val lef = query(a, b, 2 * k + 1, l, mid)
    val rig = query(a, b, 2 * k + 2, mid, r)
    if (lef._1 <= rig._1) return lef
    return rig
  }
}

object SegmentTreeTest {
  def Test() {
    val n = 128 * 128
    val arr = Array.fill(n)(0)
    val rng = Random
    for (i <- 0 until n) arr(i) = rng.nextInt()
    val seg = new SegmentTree(arr)
    val t = 1000
    var time: Long = 0
    for (_ <- 0 until t) {
      var l = rng.nextInt(n)
      var r = rng.nextInt(n)
      if (r < l) { var tmp = l; l = r; r = tmp }
      r += 1
      var min = Int.MaxValue
      var ind = -1
      for (i <- l until r) {
        if (arr(i) < min) {
          min = arr(i)
          ind = i
        }
      }
      var q = (0, 0)
      time += Utility.measureTime {
        q = seg.query(l, r)
      }
      if (q._1 != min || q._2 != ind) System.err.println("!")
    }
    System.err.println(time * 1.0 / t)
  }
}