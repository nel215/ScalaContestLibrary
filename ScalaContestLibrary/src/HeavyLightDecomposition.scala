import scala.collection.mutable.Stack
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class HeavyLightDecomposition(_tree: Array[Array[Int]], edge: Array[Int], dist: Array[Int], _root: Int) {
  val root = _root
  val tree = _tree
  val n = tree.size
  val parent = Array.fill(n)(-1)
  val depth = Array.fill(n)(0)
  val size = Array.fill(n)(0)
  val heavy = Array.fill(n)(-1)

  val st = Stack((root, -2, 0))
  while (!st.isEmpty) {
    val (cur, prev, dep) = st.pop()
    if (parent(cur) == -1) {
      parent(cur) = prev
      depth(cur) = dep
      for (next <- tree(cur)) st.push((next, cur, dep + 1))
    }
  }
  for (v <- (0 until n).sortWith(depth(_) > depth(_))) {
    size(v) = 1
    for (next <- tree(v)) {
      if (next != parent(v)) {
        size(v) += size(next)
        if (heavy(v) == -1 || size(next) > size(heavy(v))) heavy(v) = next
      }
    }
  }

  val ps = ArrayBuffer[PartialSum]()
  val seg = ArrayBuffer[SegmentTree]()
  val ver = ArrayBuffer[ArrayBuffer[Int]]()
  val chain, head = Array.fill(n)(0)
  var c = 0
  for (i <- 0 until n) {
    if (parent(i) < 0 || heavy(parent(i)) != i) {
      // ƒpƒX‚Ìe
      val e, d, v = ArrayBuffer.fill(0)(0)
      var k = i
      while (k != -1) {
        e += edge(k)
        d += dist(k)
        v += k
        chain(k) = c
        head(k) = i
        k = heavy(k)
      }
      ps += new PartialSum(d.toArray)
      seg += new SegmentTree(e.toArray)
      ver += v
      c += 1
    }
  }

  def lca(v: Int, u: Int): Int = {
    @tailrec
    def rec(v: Int, u: Int): Int = {
      if (chain(v) == chain(u)) if (depth(v) < depth(u)) v else u
      else {
        if (depth(head(v)) > depth(head(u))) rec(parent(head(v)), u)
        else rec(v, parent(head(u)))
      }
    }
    rec(v, u)
  }
  def dist(v: Int, u: Int): Int = {
    @tailrec
    def rec(v: Int, u: Int, res: Int): Int = {
      if (chain(v) == chain(u)) {
        val start = Math.min(depth(u), depth(v)) - depth(head(v))
        val end = Math.max(depth(u), depth(v)) - depth(head(v)) + 1
        res + ps(chain(v)).query(start, end)
      } else {
        if (depth(head(v)) > depth(head(u))) {
          rec(parent(head(v)), u, res + ps(chain(v)).query(depth(v) - depth(head(v)) + 1))
        } else {
          rec(v, parent(head(u)), res + ps(chain(u)).query(depth(u) - depth(head(u)) + 1))
        }
      }
    }
    rec(v, u, 0)
  }
  def rmq(v: Int, u: Int): Int = {
    if (v == u) return -1
    @tailrec
    def rec(v: Int, u: Int, res: Int): Int = {
      if (chain(v) == chain(u)) {
        val start = Math.min(depth(u), depth(v)) - depth(head(v))
        val end = Math.max(depth(u), depth(v)) - depth(head(v)) + 1
        Math.min(res, seg(chain(v)).query(start, end)._1)
      } else {
        if (depth(head(v)) > depth(head(u))) {
          rec(parent(head(v)), u, Math.min(res, seg(chain(v)).query(0, depth(v) - depth(head(v)) + 1)._1))
        } else {
          rec(v, parent(head(u)), Math.min(res, seg(chain(u)).query(0, depth(u) - depth(head(u)) + 1)._1))
        }
      }
    }
    rec(v, u, 1 << 28)
  }
  def level(v: Int, d: Int): Int = {
    @tailrec
    def rec(v: Int): Int = {
      if (d >= depth(head(v))) ver(chain(v))(d - depth(head(v)))
      else rec(parent(head(v)))
    }
    return rec(v)
  }
}