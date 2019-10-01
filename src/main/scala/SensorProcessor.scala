import java.io.File

import cats.implicits._
import models.{Sensor, SensorData, SensorStatistics}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object SensorProcessor {

  implicit val ex =  ExecutionContext.global

  def readFile(fileName: String) = {
    val source = io.Source.fromFile(fileName)
    val lines = source.getLines
    lines.drop(1)
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
      d.listFiles.filter(_.isFile).toSeq
    } else {
      Seq.empty
    }
  }

  def getData(path: String) = {
    val filePaths = getListOfFiles(path)
    Future.sequence (
      filePaths.map( path => Future {
        readFile(path.getPath)
          .map(parseLine)
          .collect{case Some(v) => v}
          .foldLeft(SensorStatistics.empty)((stat, nextData) => stat.update(nextData))
      }))
      .map(l => (l.size, l.fold(SensorStatistics.empty)((s1, s2) => SensorStatistics.add(s1, s2))))
  }

  def main(args: Array[String]): Unit = {
    val path = args(0)
    val ftr = getData(path)
    Await.ready(ftr.map(result => {
      println(s"Num of processed files: ${result._1}")
      println(result._2.counter)
      println("Sensors with highest avg humidity:")
      result._2.sensors.values.toSeq.sortWith(Sensor.compare).map(println)
    }
    ), Duration.Inf)
  }
}
