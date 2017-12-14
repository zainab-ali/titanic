package titanic
package hypothesis

import titanic.model._

/**
  * The hypothesis that all passengers will die regardless of their features.
  *
  * Most of the passengers died, so this will perform better than a completely random
  * prediction.
  */
object EveryoneDies

trait EveryoneDiesHypothesis {

  implicit val everyoneDiesHypothesis: Hypothesis[EveryoneDies.type] =
    new Hypothesis[EveryoneDies.type]{

    /** We don't use any features of the passenger to make a prediction */
    type Example = Unit

    def extract(row: TestDataRow): Example = ()

    def predict(h: EveryoneDies.type)(example: Example): Label = Label.Died
  }
}
