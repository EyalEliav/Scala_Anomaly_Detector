import scala.::
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.math.log

object Util {

  def max[T](lst: List[T], comparator: (T, T) => Int): T = {
    lst.reduce((x, y) => if (comparator(x, y) >= 0) x else y)
  }

  def map[A, B, C](lst: List[A], func1: (A) => B, func2: (B) => C): List[C] = lst.map(func1).map(func2)

  @tailrec
  def isSorted[A](lst: List[A], comparator: (A, A) => Boolean): Boolean = {
    if (lst.size == 1) true
    else if (!comparator(lst(0), lst(1))) false else isSorted(lst.drop(1), comparator)
  }

  def probs(xs:Array[Double]):Array[Double]=
    xs.map(xi=>xs.count(xj=>xi==xj)/xs.size.toDouble)

  def entropy(xs:Array[Double]):Double=
    -(xs zip probs(xs)).distinct.map(pi=>pi._2*log(pi._2)/log(2)).sum

  def entropyDifference(arr:Array[Double], index: Int): Double = {
    val without = arr.to[ListBuffer]
    without.remove(index)
    entropy(arr) - entropy(without.toArray)
  }


  def mu(arr: Array[Double]): Double = arr.zip(probs(arr)).distinct.map(x => x._1 * x._2).sum

  def variance(arr: Array[Double]): Double = arr.zip(probs(arr)).distinct.map(x => x._2 * Math.pow(x._1 - mu(arr), 2)).sum

  def zscore(arr: Array[Double], num: Double): Double = (num - mu(arr)) / Math.sqrt(variance(arr))

  def zscoreAbs(arr: Vector[Double], num: Double): Double = math.abs(num - mu(arr.toArray)) / Math.sqrt(variance(arr.toArray))

  def cov(xs: Array[Double], ys: Array[Double]): Double = mu(xs.zip(ys).map(x => (x._1 - mu(xs)) * (x._2 - mu(ys))))

  def pearson(xs: Array[Double], ys: Array[Double]): Double = cov(xs, ys) / Math.sqrt(variance(xs) * variance(ys))

  def pearsonAbs(xs: Vector[Double], ys: Vector[Double]): Double = Math.abs(pearson(xs.toArray, ys.toArray))

  def readFromFile(fileName: String): Vector[String] = {
    val source = Source.fromFile(fileName)
    try source.getLines().toVector finally source.close()
  }

  def getFeaturesNames(csvFileName:String): Vector[String] = readFromFile(csvFileName).head.split(",").toVector

  def getFeaturesValues(csvFileName:String): Vector[Vector[Double]] = readFromFile(csvFileName).tail.map(s => s.split(",").map(s => s.toDouble).toVector)

  def getFeaturesNamesToValues(csvFileName: String): Map[String, Vector[Double]] = getFeaturesNames(csvFileName).zip(getFeaturesValues(csvFileName).transpose).toMap


  def maxCorFeat(normal: TimeSeries): Seq[(String, String, Double)] = {
    for {
      featureCombs <- normal.features.combinations(2).toVector.groupBy(_ (0)).values.toVector
      maxCorFeat = featureCombs.maxBy(v => Util.pearsonAbs(normal.get(v(0)), normal.get(v(1))))
      maxCor = Util.pearsonAbs(normal.get(maxCorFeat(0)), normal.get(maxCorFeat(1)))
    } yield (maxCorFeat(0), maxCorFeat(1), maxCor)
  }




  def learnZforHyb(feat: String, normal: TimeSeries): (String, String) = {
    val column = normal.get(feat)
    val max = column.map(Util.zscoreAbs(column, _)).max
    (feat, "3," + max.toString)
  }




  def detectZforHyb(feat:String, vals: String, test:TimeSeries): Vector[(String, Int)] = {
    val column = test.get(feat)
    val max = vals.split(",")(1).toDouble
    for {
      (value, index) <- column.zipWithIndex
      if Util.zscoreAbs(column, value) > max
    } yield (feat, index)
  }

  def splitFeaturesList(vals: Vector[Double], chunckSize: Int): Vector[Vector[Double]] = {
    if (vals.length < chunckSize * 2)
      return Vector(vals)
    val splitted = vals.splitAt(chunckSize)
    splitted._1 +: splitFeaturesList(splitted._2, chunckSize)
  }
}
