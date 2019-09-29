package models

case class Counter(filesCount: Int, entriesCount: Int, NaNCount: Int) {
  override def toString: String =
    s"""
      |Num of processed files: $filesCount
      |Num of processed measurements: $entriesCount
      |Num of failed measurements: $NaNCount
      |""".stripMargin
}