import scala.annotation.tailrec

class WaveletMatrix(arr: Array[Int], _lgn: Int) {
  val (n,lgn) = (arr.size,_lgn)
  val que,zero,one = new Array[Int](n)
  val bitVector = new Array[BitVector](lgn)
  
  for (i <- 0 until n) que(i) = arr(i)
  for (i <- (0 until lgn).reverse) {
    var (os, zs) = (0, 0)
    bitVector(i) = new BitVector(n)
    for (j <- 0 until n) {
      val a = que(j)
      val b = a >> i & 1
      if (b == 1) { one(os) = a; os += 1 }
      else { zero(zs) = a; zs += 1 }
      bitVector(i).set(j, b)
    }
    bitVector(i).build()
    var qs = 0
    for (j <- 0 until zs) { que(qs) = zero(j); qs += 1 }
    for (j <- 0 until os) { que(qs) = one(j); qs += 1 }
  }
  def LessCount(l: Int, r: Int, v: Int): Int = {
    @tailrec
    def rec(l: Int, r: Int, s: Int, res: Int): Int = {
      if (s == -1) return res
      val lo = bitVector(s).rank(l, 1)
      val lz = l - lo
      val ro = bitVector(s).rank(r, 1)
      val rz = r - ro
      val z = bitVector(s).rank(n, 0)
      if ((v >> s & 1) == 1) rec(z + lo, z + ro, s - 1, res + rz - lz)
      else rec(lz, rz, s - 1, res)
    }
    return rec(l, r, lgn - 1, 0)
  }
  def GreaterCount(l: Int, r: Int, v: Int): Int = {
    @tailrec
    def rec(l: Int, r: Int, s: Int, res: Int): Int = {
      if (s == -1) return res
      val lo = bitVector(s).rank(l, 1)
      val lz = l - lo
      val ro = bitVector(s).rank(r, 1)
      val rz = r - ro
      val z = bitVector(s).rank(n, 0)
      if ((v >> s & 1) == 1) rec(z + lo, z + ro, s - 1, res)
      else rec(lz, rz, s - 1, res + ro - lo)
    }
    return rec(l, r, lgn - 1, 0)
  }
}