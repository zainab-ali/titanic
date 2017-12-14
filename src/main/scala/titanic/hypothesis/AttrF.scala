package titanic
package hypothesis

import scalaz._, Scalaz._
import matryoshka.data._

/**
  * Tags each node and leaf of the tree with an attribute a
  * 
  */
case class AttrF[A, B](a: A, tree: TreeF[B])

object AttrF {

  /**
    * Scalaz functor for a tree
    * 
    */
  implicit def attrFunctor[C]: Functor[AttrF[C, ?]] =
    new Functor[AttrF[C, ?]] {

      def map[A, B](fa: AttrF[C, A])(f: A => B): AttrF[C, B] =
       AttrF(fa.a, fa.tree.map(f))
  }

  /**
    * Removes the tags from the tree
    * 
    */
  def untag[A]: AttrF[A, Tree] => Tree = attr => Fix(attr.tree)
}
