import scala.math.log

package object titanic {

  /** Calculates the log base 2 of a value */
  def log2(x: Double): Double = log(x) / log(2)

  implicit final class MapOps[K, A](map: Map[K, A]) {

    /** 
      * Maps over the values of a map.
      *
      * Scala's mapValues method is deceptively lazy.  This version is eager.
      * 
      */
    def mapv[B](f: A => B): Map[K, B] = map.map {
      case (k, v) => (k, f(v))
    }
  }
}
