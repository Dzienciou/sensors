package models

import cats.Semigroup
import cats.implicits._

case class SensorStatistics(counter: Counter, sensors: Map[String, Sensor]) {

  def update(nextData: SensorData) = {
    SensorStatistics(
      Counter(counter.entriesCount+1, if (nextData.value.isEmpty) counter.NaNCount+1 else counter.NaNCount),
      sensors.updated(
        nextData.id,
        sensors.getOrElse(nextData.id, Sensor.empty(nextData.id))
          .addValue(nextData.value))
    )
  }
}

object SensorStatistics {

  implicit val sensorSemigroup: Semigroup[Sensor] = (x: Sensor, y: Sensor) => Sensor.add(x, y)

  def empty() = SensorStatistics(Counter(0, 0), Map.empty)

  def add(s1:SensorStatistics, s2:SensorStatistics): SensorStatistics =
    SensorStatistics(Counter.sum(s1.counter, s2.counter), s1.sensors |+| s2.sensors)
}