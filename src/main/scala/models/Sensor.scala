package models

import cats.Semigroup

import scala.math.{max, min}
import cats.implicits._



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
  def add(s1: Sensor, s2: Sensor) =
    Sensor(
      s1.id,
      if (s1.sum.isDefined || s2.sum.isDefined) Some(s1.sum.getOrElse(0) + s2.sum.getOrElse(0)) else None,
      s1.count + s2.count,
      if (s1.maxVal.isDefined || s2.maxVal.isDefined) Some(max(s1.maxVal.getOrElse(-1), s2.maxVal.getOrElse(-1))) else None,
      if (s1.minVal.isDefined || s2.minVal.isDefined) Some(min(s1.minVal.getOrElse(101), s2.minVal.getOrElse(101))) else None,
    )
}


