package titanic

import matryoshka.data.Fix
import matryoshka.patterns.ListF
import matryoshka.implicits._
import scalaz._, Scalaz._

import titanic.model._

package object hypothesis extends EveryoneDiesHypothesis
    with FemalesSurviveHypothesis {

  type Tree = Fix[TreeF]

  implicit def treeHypothesis: Hypothesis.Aux[Tree, Example] =
    new Hypothesis[Tree] {

      type Example = titanic.model.Example

      def extract(row: TestDataRow): Example = titanic.model.Example(
        gender = extraction.gender(row),
        age = extraction.age(row),
        ticketClass = extraction.ticketClass(row),
        familySize = extraction.familySize(row),
        hasCabin = extraction.hasCabin(row),
        port = extraction.port(row)
      )

      def predict(tree: Tree)(example: Example): Label =
        tree.hylo[Label Either ?, Label](_.merge, TreeF.explore(example))
    }

  type Counts = Map[Label, Int]
  type AttrFCount[A] = AttrF[Counts, A]
  type AttrFCostInfo[A] = AttrF[Prune.CostInfo, A]

  /** Type alias for inputs to tree building process */
  type Input = (List[TrainingExample[Example]], List[Feature[Example]])


  type SubtreeListF[A] = ListF[Fix[AttrFCostInfo], A]
}
