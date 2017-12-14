package titanic
package hypothesis

import titanic.model._

/**
  * Typeclass for a hypothesis.
  *
  * A hypothesis is a mapping from a set of examples to a set of labels.
  * Examples are extracted from the raw data, and can vary per hypothesis.
  */
trait Hypothesis[H] {

  /**
    * The Example type.
    *
    * An example is a set of features and values needed to make a prediction.
    */
  type Example

  /**
    * Extracts an example from a row of raw data.  Not all of the raw data is used to
    * make the prediction.
    *
    * @param row The row of raw data corresponding to a passenger
    * @returns   The set of features and values of the passenger needed to make a
    *            prediction
    */
  def extract(row: TestDataRow): Example

  /**
    * Extracts a training example from raw data.
    *
    * This is a pair of an example and label used to train the hypothesis.
    *
    * @param row The row of raw data corresponding to a passenger and label
    * @returns   The training example for the passenger
    */
  def extract(row: RawDataRow): TrainingExample[Example] =
    TrainingExample(extract(row.testData), row.label)

  /**
    * Predicts a label for an example.  This is the core part of a hypothesis.
    *
    * @param example The set of features and values of a passenger needed to make a
    *                prediction
    * @returns       A prediction of whether the passenger survived or died
    */
  def predict(hypothesis: H)(example: Example): Label


  /** Calculates the labels for a set of test data */
  def predictAll(hypothesis: H)(data: List[TestDataRow]): Map[Int, Label] =
    data.map(row => row.passengerId -> predict(hypothesis)(extract(row))).toMap
}

object Hypothesis {

  type Aux[H, E] = Hypothesis[H] { type Example = E }
}
