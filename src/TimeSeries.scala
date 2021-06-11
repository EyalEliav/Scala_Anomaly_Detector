

class TimeSeries(fts: Vector[String], ftv: Map[String, Vector[Double]]) {
  val features: Vector[String] = fts

  val featuresToVal: Map[String, Vector[Double]] = ftv

  def this(csvFileName: String){
    this(Util.getFeaturesNames(csvFileName), Util.getFeaturesNamesToValues(csvFileName))
  }


  // given name of a feature return in O(1) its value series
  def getValues(feature: String): Option[Vector[Double]] = featuresToVal.get(feature)
  def get(feature: String): Vector[Double] = featuresToVal(feature)

  // given name of a feature return in O(1) its value at the given time step
  def getValue(feature: String, timeStep: Int): Option[Double] = {
    featuresToVal.get(feature) match {
      case Some(col) => col.lift(timeStep)
      case None => None
    }
  }

  // given name of a feature return its value series in the range of indices
  def getValues(feature: String, r: Range): Option[Vector[Double]] = getValues(feature).filter(v=> r.start >= 0 && v.length > r.end).map(_.slice(r.start, r.end + 1))


  def length(): Int = featuresToVal(features(0)).length

  def split(n: Int): List[TimeSeries] = {
    val split = features.map(f => Util.splitFeaturesList(get(f), get(f).length / n))
    (for (i <- 0 until n) yield {
      val subArr = features.zip(split.map(t=> t(i))).toMap
      new TimeSeries(features, subArr)
    }).toList
  }

}
