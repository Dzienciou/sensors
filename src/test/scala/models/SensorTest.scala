package models

class SensorTest extends org.specs2.mutable.Specification {
  "Sensor" should {
    "Return correct values for input SensorValues" in {
      val s1 = Sensor("s1", None, 0, None, None)
      val s2 = s1.addValue(Some(10))
      val s3 = s2.addValue(Some(5))

      s3.toString should_== "s1, 7.5, 10, 5"
    }
    "Return correct values for Nan inputs" in {
      val s1 = Sensor("s1", None, 0, None, None)
      val s2 = s1.addValue(None)

      s2.toString should_== "s1, NaN, NaN, NaN"
    }
  }
}
