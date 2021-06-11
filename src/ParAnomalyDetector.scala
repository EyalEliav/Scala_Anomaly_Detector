import java.util.concurrent.{Callable, ExecutorService}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future


case class Report(feature: String, var timeStep: Int, anomalyScore: Double)


trait ParAnomalyDetector {
  type Reports = ListBuffer[Report]
  def map(ts: TimeSeries): Reports
  def reduce(r1: Reports, r2: Reports): Reports

  // implement
  def detect(ts: TimeSeries, es: ExecutorService, chunks: Int): Vector[Report] = {
    val chunksize = ts.length() / chunks
    val futures =  ts.split(chunks).map(p => es.submit(new Callable[Reports] {
      override def call(): Reports = map(p)
    }))
    futures.zipWithIndex.map(p => p._1.get().map(r => Report(r.feature, p._2 * chunksize + r.timeStep, r.anomalyScore))).reduce((a,b) => reduce(a,b)).toVector
  }
}

