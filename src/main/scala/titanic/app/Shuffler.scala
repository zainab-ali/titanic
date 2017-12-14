package titanic.app


import java.io.File

import purecsv.unsafe._
import scala.util.Random
import scala.io.Source

import titanic.Config
import titanic.model.{PreshuffledRow, RawDataRow}

/**
  * Shuffles the original data.
  *
  * Each passenger is given a unique id. Passengers are split into training and test
  * data.
  */
object Shuffler {

  val random = new Random(42)

  def shuffle(): Unit = {
    val rows = CSVReader[PreshuffledRow].readCSVFromFileName(
      Config.preshuffledFileName, skipHeader = true)
    val shuffled = random.shuffle(rows)

    val rawData = shuffled.zipWithIndex.map {
      case (row, i) =>
        RawDataRow(
          passengerId = i,
          pclass = row.pclass,
          name = row.name,
          gender = row.gender,
          age = row.age,
          sibsp = row.sibsp,
          parch = row.parch,
          ticket = row.ticket,
          fare = row.fare,
          cabin = row.cabin,
          embarked = row.embarked,
          homeDest = row.homeDest,
          survived = row.survived
        )
    }

    val trainingData = rawData.drop(Config.testSize)
    val testData = rawData.take(Config.testSize)

    trainingData.writeCSVToFileName(Config.trainingFileName)
    testData.writeCSVToFileName(Config.testFileName)
  }

  def main(args: Array[String]) = {
    shuffle()
  }
}
