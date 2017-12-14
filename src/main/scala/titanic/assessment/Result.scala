package titanic
package assessment

/**
  * All results of assessing a hypothesis.
  *
  * @param risk  The emprical risk of the hypothesis
  * @param noise The noise of the examples of the hypothesis.
  * 
  */
case class Result(
  risk: Double,
  noise: Double
)
