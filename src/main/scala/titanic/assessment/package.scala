package titanic

import titanic.model._
import titanic.hypothesis._

import scala.math.min

package object assessment {


  /**
    * Calculates the empirical risk of a model on a sample.
    * 
    * This is also known as the empirical error, or empirical generalization error.
    * It is a measure of how well a model predicts the outcomes in a sample.
    *
    * The expectation value of the empirical risk across all samples in a population is
    * the true risk, or generalization error.
    * 
    * @param predictions Predictions for all examples in a sample.  This must be the
    *                    same size as the sample.
    * @param actual      The actual label for all examples in the sample.  This must
    *                    have a row for every entry in the predictions.  It must be the
    *                    same size as the sample.
    */
  def empiricalRisk(predictions: Map[Int, Label], actual: Map[Int, Label]): Double = {
    val m = predictions.size.toDouble
    val Σ = predictions.map {
      case (passengerId, prediction) => indicator(prediction, actual(passengerId))
    }.sum
    Σ / m
  }

  /**
    * The indicator function
    *
    * This measures how well a prediction matches up to the actual label.
    * The expectation value of indicators is a measure of risk.
    * 
    */
  def indicator(predicted: Label, actual: Label): Double =
    if (predicted != actual) 1.0 else 0.0

  /**
    * The probability that a label in the given list is the "wrong" label.
    */
  private [assessment] def pmin(labels: List[Label]): Double = {
    val total = labels.size.toDouble
    val survived = labels.filter(_ == Label.Survived).size.toDouble
    val died = labels.filter(_ == Label.Died).size.toDouble

    min(survived, died) / total
  }


  /**
    * A measure of the impurity of the data.
    *
    * For stochastic, inconsistent datasets, instances of the same example can give
    * different labels.  On the Titanic, a passenger may have survived, while one with
    * exactly the same features may have died.  This limits the prediction ability of
    * any possible hypothesis.
    */
  def noise[E](examples: List[(E, Label)]): Double = {
    val groups = examples.groupBy(_._1)
    val m = groups.size.toDouble
    val Σ = groups.map {
      case (e, examples) => pmin(examples.map(_._2).toList)
    }.sum
    Σ / m
  }

  def assess[H](hypothesis: H)(testRows: List[TestDataRow], actual: Map[Int, Label])(
    implicit H: Hypothesis[H]) = {
    val predictions = H.predictAll(hypothesis)(testRows)
    val risk = empiricalRisk(predictions, actual)

    val examples = testRows.map(r => H.extract(r) -> actual(r.passengerId))
    val n = noise(examples)
    Result(risk, n)
  }

}
