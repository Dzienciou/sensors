package models

import scala.math.{max,min}
import cats.implicits._

case class SensorData(id: String, value: Option[Int])

case class Sensor(id: String, sum: Option[Int], count: Int, maxVal: Option[Int], minVal: Option[Int]) {
  def addValue(value: Option[Int]) =
    value match {
      case newVal @ Some(v) =>
        Sensor(
          id,
          sum.fold(newVal)(oldV => Some(oldV + v)),
          count+1,
          sum.fold(newVal)(oldV => Some(max(oldV,v))),
          sum.fold(newVal)(oldV => Some(min(oldV,v)))
        )
      case None => this.copy()
    }

  override def toString: String = {
    val avg = sum.map(_.toDouble / count)
    (avg, maxVal, minVal).mapN((avgV, maxV, minV) => s"$id,$avgV,$maxV,$minV")
      .getOrElse(s"$id,NaN,NaN,NaN")
  }
}

object Sensor {
  def empty(id: String) = Sensor(id, None, 0, None, None)

  def compare(s1: Sensor, s2: Sensor) = {
    val avg1: Double = s1.sum.map(_.toDouble / s1.count).getOrElse(-1.0)
    val avg2: Double = s2.sum.map(_.toDouble / s2.count).getOrElse(-1.0)
    avg1 > avg2
  }
}