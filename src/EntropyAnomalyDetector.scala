import scala.collection.mutable.ListBuffer

object EntropyAnomalyDetector extends ParAnomalyDetector{


  override def map(ts: TimeSeries):Reports={
    (for {
      f <- ts.features
      score = ts.get(f).zipWithIndex.map(p => (Util.entropyDifference(ts.get(f).toArray, p._2), p._2))
      max = score.maxBy(p => p._1)
    } yield Report(f, max._2, max._1)).to[ListBuffer]
  }

  override def reduce(r1:Reports,r2:Reports):Reports= r1.zip(r2).map(p => if (p._1.anomalyScore > p._2.anomalyScore) p._1 else p._2)
}