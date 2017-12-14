package titanic
package hypothesis

import titanic.model._

/**
  * The hypothesis that only female passengers survive.
  *
  * Based on analysis of the training data, far more females survived than males.
  */
object FemalesSurvive

trait FemalesSurviveHypothesis {

  implicit val femalesSurviveHypothesis: Hypothesis[FemalesSurvive.type] =
    new Hypothesis[FemalesSurvive.type] {

      /** The only feature we need to use is gender. */
      type Example = Gender

      def extract(row: TestDataRow): Example = extraction.gender(row)

      def predict(h: FemalesSurvive.type)(example: Example): Label = example match {
        case Gender.Female => Label.Survived
        case Gender.Male => Label.Died
      }
      
    }
}

