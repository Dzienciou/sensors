package models

case class Counter(entriesCount: Int, NaNCount: Int) {
  override def toString: String =
    s"""
      |Num of processed measurements: $entriesCount
      |Num of failed measurements: $NaNCount
      |""".stripMargin
}

object Counter {
  def sum(c1: Counter, c2: Counter) = Counter(c1.entriesCount+c2.entriesCount, c1.NaNCount+c2.NaNCount)
}