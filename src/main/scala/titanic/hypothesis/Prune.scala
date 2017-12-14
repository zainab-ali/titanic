package titanic
package hypothesis

import scalaz._, Scalaz._
import matryoshka._
import matryoshka.data._
import matryoshka.implicits._
import matryoshka.patterns._

import titanic.model._
import titanic.assessment._


/**
  * Algorithms for pruning the tree using cost complexity pruning.
  *
  * This takes the following steps:
  *
  * 1. Tag the tree with information to calculate the cost at each node.
  * 2. Find the minimum cost in the tree.
  * 3. Replace the nodes with the minimum cost with leaves.
  *
  * This process is repeated to create a series of trees.  The final tree is decided
  * using a validation dataset.
  */
object Prune {

  /**
    * The information to calculate the cost.
    *
    * @param leafCount           The number of leaves
    * @param risk                The sum of the risk of the child trees
    * @param labels              The count of labels of all of the child nodes
    */
  case class CostInfo(
    leafCount: Int,
    risk: Int,
    labels: Map[Label, Int]
  ) {

    /** The cost to minimize when pruning.
      *
      * This is a function of the number of leaves removed, the resubstitution risk and
      * the current risk. A large cost implies the removal of a large number of leaves
      * with a small increase in risk.
      *
      */
    def cost: Double = {
      if(leafCount == 1) Double.PositiveInfinity
      else (resubstitutionRisk(labels) - risk).toDouble / (leafCount - 1).toDouble
    }
  }


  /**
    * Builds a tree tagged with label counts.
    *
    * The label counts correspond to the number of passengers who survived and died
    * who have the feature value at the given node.  This is necessary for calculating
    * the resubstitution risk.
    */
  def build: Coalgebra[AttrF[Counts, ?], Input] = {
    case (examples, features) =>
      val counts = examples.groupBy(_.label).mapv {subset => subset.size}
      val tree = TreeF.build((examples, features))
      AttrF(counts, tree)
  }


  /**
    * Builds a tree from raw training data
    * 
    */
  def buildFrom(rawData: List[RawDataRow]): Fix[AttrFCount] = {
    (rawData.map(treeHypothesis.extract)
      -> features).ana.apply[Fix[AttrFCount]].apply[AttrFCount](build)
  }


  /**
    * The resubstitution risk.
    *
    * If the node was to be substituted for a leaf, this is the risk of the leaf.
    * It corresponds to the count of incorrectly classified values.
    *
    * This calculation is technically not the resubstitution risk, but is proportional
    * to it. * The resubstitution risk can be obtained by dividing this by the total
    * number of passengers.
    */
  def resubstitutionRisk(labels: Map[Label, Int]): Int = {
    val label: Label = labels.maxBy(_._2)._1
      (labels - label).values.sum
  }

  /**
    * Algebra to tag a tree with cost info.  This can be used to calculate the cost at
    * each node.
    */
  def tagCostInfoAlgebra: Algebra[AttrF[Counts, ?], Fix[AttrFCostInfo]] =
    t => t match {
      case AttrF(labels, t: Leaf[Fix[AttrFCostInfo]]) =>
        Fix[AttrF[CostInfo, ?]](AttrF(leafCostInfo(labels), t))
      case AttrF(labels, t @ Node(_, children)) =>
        val childCostInfo = children.values.map(_.unFix.a).toList
        val attr = AttrF(parentCostInfo(labels, childCostInfo), t)
        Fix[AttrF[CostInfo, ?]](attr)
    }

  /**
    * Calculates the cost info for a parent given the info of its children.

    * This counts the number of leaves stemming from the node.
    * The risk at the node is the sum of the risk of all of its children.
    * 
    */
  def parentCostInfo(labels: Counts, children: List[CostInfo]): CostInfo = {
      val leafCount = children.map(_.leafCount).sum
      val risk = children.map(_.risk).sum
      CostInfo(leafCount, risk, labels)
  }

  /**
    * Calculates the cost info for a leaf given its labels.
    *
    *  This information is useful in calculating the cost info for parent nodes.
    */
  def leafCostInfo(labels: Counts): CostInfo = {
    CostInfo(1, resubstitutionRisk(labels), labels)
  }

  /**
    * Finds the minimum cost in the tree.
    */
  def minCost: Algebra[AttrF[CostInfo, ?], Double] = {
    case AttrF(_, Leaf(_)) => Double.PositiveInfinity
    case AttrF(costInfo, Node(_, children)) =>
     (costInfo.cost :: children.values.toList).min
  }

  /**
    * Prunes nodes with the minimum cost.
    *
    * The cost info is updated with the new leaf count.
    *
    * @param value  The minimum cost.  Nodes with this cost are pruned.
    */
  def prune(value: Double): Algebra[AttrF[CostInfo, ?], Fix[AttrFCostInfo]] = {
    case  attr @ AttrF(c, Leaf(l)) =>
      Fix[AttrF[CostInfo, ?]](attr)
    case AttrF(c, n @ Node(_, children)) =>
      if(c.cost <= value) {
        val nextInfo = leafCostInfo(c.labels)
        Fix[AttrF[CostInfo, ?]](AttrF(nextInfo, Leaf(c.labels.maxBy(_._2)._1)))
      } else {
        Fix[AttrF[CostInfo, ?]](AttrF(
          parentCostInfo(c.labels, children.values.map(_.unFix.a).toList), n)
        )
      }
  }

  /**
    * Creates a list of pruned subtrees starting from a greedy tree.
    * 
    */
  val prunedSubtrees: Coalgebra[ListF[Fix[AttrFCostInfo], ?], Fix[AttrFCostInfo]] = {
    case Fix(AttrF(_, Leaf(_))) => NilF()
    case tree =>
      val subtree = tree.cata(prune(tree.cata(minCost)))
      ConsF(subtree, subtree)
  }

  /**
    * Finds the subtree with the lowest risk
    * 
    */
  def bestSubtree(tree: Fix[AttrFCostInfo], validationData: List[TestDataRow],
    validationLabels: Map[Int, Label]):
      Algebra[ListF[Fix[AttrFCostInfo], ?], (Double, Tree)] = {
  case NilF() =>
      val untagged = tree.cata(AttrF.untag)
    val risk = assess(untagged)(validationData, validationLabels).risk
    (risk, untagged)
  case ConsF(subtree, (currentMin, current)) =>
      val t: Fix[AttrFCostInfo] = subtree
      val untagged = t.cata(AttrF.untag)
    val risk = assess(untagged)(validationData, validationLabels).risk
    if(risk < currentMin) (risk, untagged)
    else (currentMin, current)
}

  /**
    * Prunes the tree using cost complexity pruning.
    *
    * Creates a list of pruned subtrees, each one smaller than the other, until the root
    * is reduced to a leaf.
    *
    * The subtrees are assessed on a validation dataset and the tree with the lowest
    * risk is chosen.
    * 
    */
  def prune(tree: Fix[AttrFCostInfo], validationRows: List[TestDataRow],
    validationLabels: Map[Int, Label]): Tree = {
    val subtrees = tree.ana.apply[Fix[SubtreeListF]].apply[SubtreeListF](prunedSubtrees)
    subtrees.cata(bestSubtree(tree, validationRows, validationLabels))._2
  }
}
