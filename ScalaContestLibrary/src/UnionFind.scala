class UnionFind(n: Int) {
  val data = Array.fill(n)(-1)
  def unionSet(_x: Int, _y: Int): Boolean = {
    var (x, y) = (Root(_x), Root(_y))
    if (x != y) {
      if (data(y) < data(x)) return unionSet(_y, _x)
      data(x) += data(y)
      data(y) = x
    }
    x != y
  }
  def Root(x: Int): Int = if (data(x) < 0) x else { data(x) = Root(data(x)); data(x) }
  def Size(x: Int): Int = -data(Root(x))
}