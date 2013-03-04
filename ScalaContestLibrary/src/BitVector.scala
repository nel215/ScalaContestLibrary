class BitVector(n: Int) {
  private val W = 32
  private val M = W - 1
  private val vec = new Array[Int]((n + W) / W)
  private val sum = new Array[Int](vec.size + 1)
  private def popcount(b: Int): Int = {
    var x = b
    x -= (x >> 1) & 0x55555555
    x = (x & 0x33333333) + ((x >> 2) & 0x33333333)
    x = (x + (x >> 4)) & 0x0f0f0f0f
    return (x * 0x01010101) >> 24
  }
  def set(i: Int, v: Int) = {
    if (v == 1) vec(i / W) = vec(i / W) | (1 << (i & M))
  }
  def build() = {
    for (i <- 0 until vec.size) sum(i + 1) = sum(i) + popcount(vec(i))
  }
  def rank(i: Int, v: Int): Int = {
    val mask = (1 << (i & M)) - 1
    val res = sum(i / W) + popcount(vec(i / W) & mask)
    if (v == 1) res else i - res
  }
}