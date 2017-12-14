package titanic
package hypothesis

import scala.math.log

import scalaz.Functor
import matryoshka._
import matryoshka.implicits._


import titanic.model._

/**
  * A 'TreeF' is a pattern functor for a tree.
  *
  * The fixed point of this functor is a recursive data structure.
  */
sealed trait TreeF[A]
case class Leaf[A](label: Label) extends TreeF[A]
case class Node[A](feature: Feature[Example], children: Map[String, A]) extends TreeF[A]

object TreeF {

  /**
    * Scalaz functor for a tree.
    *
    */
  implicit val treeFunctor: Functor[TreeF] = new Functor[TreeF] {
    def map[A, B](fa: TreeF[A])(f: A => B): TreeF[B] = fa match {
      case Leaf(l) => Leaf(l)
      case Node(feature, children) => Node(feature, children.mapv(f))
    }
  }

  /**
    * Calculates the entropy for a list of labels.
    *
    * This is a measure of the variation of labels in a dataset.
    * When all labels are the same (all survived or all died), the entropy is 0.0
    * When there are equal amounts of survived and died labels, the entropy is 1.0
    */
  private def entropy(labels: List[Label]): Double = {
    val m = labels.size.toDouble
    val grouped = labels.groupBy(identity)
    grouped.map {
      case (_, subset) =>
        val plabel = subset.size.toDouble / m
        if(plabel > 0.0) plabel * log2(1.0 / plabel) else 0.0
    }.sum
  }


  /**
    * The decrease in entropy caused by splitting the sample on a given feature.
    * A large gain corresponds to a large decrease in entropy.
    */
  private def gain[E](sample: List[TrainingExample[E]], feature: Feature[E]): Double = {
    val current = entropy(sample.map(_.label))
    val grouped = sample.groupBy (s => feature(s.example))
    val next = grouped.map {
      case (_, subset) =>
        val pvalue = subset.size.toDouble / sample.size.toDouble
        pvalue * entropy(subset.map(_.label))
    }.sum
    current - next
  }

  /**
    * Finds the most common label from a list of examples
    *
    */
  def mostCommonLabel(examples: List[TrainingExample[Example]]): Label =
    examples.groupBy(_.label).maxBy {
    case (_, subset) => subset.size
  }._1


  /**
    * Coalgebra for constructing a tree.
    *
    * This finds the feature with the maximum gain, and creates a node for that feature.
    * Construction terminates when there are no more features, or there is no more gain.
    */
  val build: Coalgebra[TreeF, Input] = {
    case (examples, features) =>

      // There are no more features.  Create a leaf with the most common label
      if (features.isEmpty) {
        Leaf(mostCommonLabel(examples))

      // There is only one label.  Create a leaf with that label
      } else if(examples.map(_.label).toSet.size <= 1) {
        Leaf(mostCommonLabel(examples))
      } else {
        // Find the feature with the biggest gain
        val (feature, maxGain) = features.map(f => (f, gain(examples, f)))
          .maxBy(_._2)

        // There is no benefit in creating additional nodes
        if (maxGain == 0.0) {
          Leaf(mostCommonLabel(examples))
        } else {
          val nextFeatures = features.filterNot(_ == feature)
          // Create a node with children for each feature value
          val grouped = examples.groupBy(t => feature(t.example))
          Node(feature,
            grouped.mapv(subset => (subset, nextFeatures)))
        }
      }
  }

  /**
    * Builds a tree from raw training data
    * 
    */
  def buildFrom(rawData: List[RawDataRow]): Tree =
    (rawData.map(treeHypothesis.extract) -> features).ana[Tree](TreeF.build)


  /** Coalgebra for building the path to a label for a passenger */
  def explore(example: Example): Coalgebra[Label Either ? , Tree] = fix =>
  fix.unFix match {
    case Leaf(label) => Left(label)
    case Node(feature, children) =>
      children.get(feature(example)).toRight(Label.Died)
  }

  /** Algebra to count the leaves in a tree */
  val countLeaves: Algebra[TreeF, Int] = {
    case Leaf(_) => 1
    case Node(_, children) => children.values.sum
  }
}
