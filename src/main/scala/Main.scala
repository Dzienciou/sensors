import java.io.File

import cats.implicits._
import models.{Counter, Sensor, SensorData}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object SensorProcessor {

  implicit val ex =  ExecutionContext.global

  def readFile(fileName: String) = {
    val source = io.Source.fromFile(fileName)
    val lines = source.getLines.toSeq
    source.close()
    lines.slice(1, lines.size)
  }

  def parseLine(line: String) = {
    val splitted = line.split(",")
    (splitted.lift(0), splitted.lift(1)).mapN(
      (id, value) => SensorData(id, value.toIntOption)
    )
  }


  def getListOfFiles(dir: String) = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      println(d.listFiles.filter(_.isFile).toSeq)
      d.listFiles.filter(_.isFile).toSeq
    } else {
      Seq.empty
    }
  }

  def getData(path: String) = {
    val filePaths = getListOfFiles(path)
    val linesFtr = Future.sequence (
      filePaths.map( path => Future { readFile(path.getPath).map(parseLine) collect {case Some(v) => v}}))

    linesFtr.map(sensorList =>
      (Counter(
        sensorList.length,
        sensorList.flatten.length,
        sensorList.flatten.map(_.value).collect{ case None => None }.length
      ),
      sensorList.flatten.foldLeft(Map.empty[String, Sensor])((m, nextData) =>
      m.updated(
        nextData.id,
        m.getOrElse(nextData.id, Sensor.empty(nextData.id))
          .addValue(nextData.value))
      )
      )
    )
  }

  def main(args: Array[String]): Unit = {
    val path = args(0)
    val ftr = getData(path)
    Await.ready(ftr.map(result => {
      println(result._1)
      println("Sensors with highest avg humidity:")
      result._2.values.toSeq.sortWith(Sensor.compare).map(println)
    }
    ), Duration.Inf)
  }
}
