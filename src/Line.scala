class Line(ps:Array[Point]) {
  val ys: Array[Double] = ps.map(p=>p.y)
  val xs: Array[Double] = ps.map(p=>p.x)
  val a: Double = Util.cov(xs, ys) / Util.variance(xs)
  val b: Double = (ys.sum / ys.length) - (a * (xs.sum / xs.length))
  // read only values a and b

  // f
  def f(num: Double): Double = a * num + b

  // dist
  def dist(p: Point): Double = Math.abs(f(p.x) - p.y)
}
