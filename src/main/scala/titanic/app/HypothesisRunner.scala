package titanic
package app

import purecsv.unsafe._

import matryoshka.implicits._
import matryoshka.data._

import titanic.assessment._
import titanic.model._
import titanic.hypothesis._

object HypothesisRunner {

  /**
    * Trains and runs the hypotheses
    */
  def main(args: Array[String]) = {

    val testRows = CSVReader[RawDataRow].readCSVFromFileName(
      Config.testFileName, skipHeader = false)

    val trainingRows = CSVReader[RawDataRow].readCSVFromFileName(
      Config.trainingFileName, skipHeader = false)

    // Build a greedy decision tree
    val greedyTree = (trainingRows.map(treeHypothesis.extract) -> features)
      .ana.apply[Tree].apply[TreeF](TreeF.build)

    // Split the data into a training and validation set
    val treeTrainingRows = trainingRows.take(Config.trainingSize)
    val treeValidationRows = trainingRows.drop(Config.trainingSize)
    val actualValidationLabels = treeValidationRows.map(r => r.passengerId -> r.label).toMap

    // Build a tagged greedy decision tree
    val taggedTree = (treeTrainingRows.map(treeHypothesis.extract)
        -> features).hylo[AttrFCount, Fix[AttrFCostInfo]](
        Prune.tagCostInfoAlgebra, Prune.build)

    // Prune the trees to create a series of subtrees
    val prunedTree = Prune.prune(taggedTree,
      treeValidationRows.map(_.testData),
      actualValidationLabels
    )

    // Test the models
    val testData = testRows.map(_.testData)
    val actualTestLabels = testRows.map(r => r.passengerId -> r.label).toMap

    val results = Map(
      "everyoneDies" -> assess(EveryoneDies)(testData, actualTestLabels),
      "femalesSurvive" -> assess(FemalesSurvive)(testData, actualTestLabels),
      "greedyDecisionTree" -> assess(greedyTree)(testData, actualTestLabels),
      "prunedTree" -> assess(prunedTree)(testData, actualTestLabels)
    )

    results.foreach {
      case (hypothesis, result) =>
        println(s"The risk  for [$hypothesis] is [${result.risk}]")
        println(s"The noise for [$hypothesis] is [${result.noise}]")
    }
  }
}
