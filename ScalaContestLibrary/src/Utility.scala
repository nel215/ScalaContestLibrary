object Utility {
  def measureTime(func: => Unit):Long = {
    val start = System.currentTimeMillis()
    func
    return System.currentTimeMillis()-start
  }
}