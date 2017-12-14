package titanic

/** Contains case classes */
package object model {


  /**
    * A row in the preshuffled dataset.
    *
    * This contains some unnecessary fields such as boat and body, which were only known
    * after the ship sunk.
    * 
    * @param pclass    The ticket class of the passenger
    * @param survived  Whether the passenger survived or died
    * @param name      The name of the passenger
    * @param gender    The gender of the passenger
    * @param age       The age of the passenger
    * @param sibsp     The number of siblings and spouses travelling with the passenger
    * @param parch     The number of parents and children travelling with the passenger
    * @param ticket    The ticket code of the passenger
    * @param fare      The price paid by the passenger
    * @param cabin     The cabin code of the passenger.  If not present, the passenger
    *                  doesn't have a cabin.
    * @param embarked  The embarkation port of the passenger
    * @param boat      The code of the boat that the passenger was saved on.  This must
    *                  not be used for prediction
    * @param body      The code of the recovered corpse.  This must not be used for
    *                  prediction
    * @param homeDest  The destination of the passenger
    */
  case class PreshuffledRow(
    pclass: Int,
    survived: Int,
    name: String,
    gender: String,
    age: Option[Double],
    sibsp: Int,
    parch: Int,
    ticket: String,
    fare: Option[Double],
    cabin: String,
    embarked: String,
    boat: String,
    body: String,
    homeDest: String
  )

  /**
    * A row in the raw dataset.  This contains whether the passenger lived or died.
    * 
    * @param passengerId The unique id of the passenger
    * @param pclass      The ticket class of the passenger
    * @param name        The name of the passenger
    * @param gender         The gender of the passenger
    * @param age         The age of the passenger
    * @param sibsp       The number of siblings and spouses travelling with the passenger
    * @param parch       The number of parents and children travelling with the passenger
    * @param ticket      The ticket code of the passenger
    * @param fare        The price paid by the passenger
    * @param cabin       The cabin code of the passenger.  If not present, the passenger
    *                    doesn't have a cabin.
    * @param embarked    The embarkation port of the passenger
    * @param homeDest    The destination of the passenger
    * @param survived    Whether the passenger survived or died
    */
  case class RawDataRow(
    passengerId: Int,
    pclass: Int,
    name: String,
    gender: String,
    age: Option[Double],
    sibsp: Int,
    parch: Int,
    ticket: String,
    fare: Option[Double],
    cabin: String,
    embarked: String,
    homeDest: String,
    survived: Int
  ) {

    /**
      * Extracts test data from a row of raw data.
      *
      * This doesn't have a label.
      * 
      */
    def testData: TestDataRow = TestDataRow(
      passengerId = passengerId,
      pclass = pclass,
      name = name,
      gender = gender,
      age = age,
      sibsp = sibsp,
      parch = parch,
      ticket = ticket,
      fare = fare,
      cabin = cabin,
      embarked = embarked,
      homeDest = homeDest
    )

    def label: Label =
      if (survived == 1) Label.Survived else Label.Died
  }

  /**
    * A row in the raw dataset without the actual label.  This is used for assessing a
    * hypothesis
    * 
    * @param passengerId The unique id of the passenger
    * @param pclass      The ticket class of the passenger
    * @param name        The name of the passenger
    * @param gender         The gender of the passenger
    * @param age         The age of the passenger
    * @param sibsp       The number of siblings and spouses travelling with the passenger
    * @param parch       The number of parents and children travelling with the passenger
    * @param ticket      The ticket code of the passenger
    * @param fare        The price paid by the passenger
    * @param cabin       The cabin code of the passenger.  If not present, the passenger
    *                    doesn't have a cabin.
    * @param embarked    The embarkation port of the passenger
    * @param homeDest    The destination of the passenger
    */
  case class TestDataRow(
    passengerId: Int,
    pclass: Int,
    name: String,
    gender: String,
    age: Option[Double],
    sibsp: Int,
    parch: Int,
    ticket: String,
    fare: Option[Double],
    cabin: String,
    embarked: String,
    homeDest: String
  )


  /**
    * A category assigned by the model.
    *
    * The labels used here are binary.  Binary problems are easier to deal with
    * than multi class ones.
    */
  sealed trait Label

  object Label {

    /** Represents the survival of a passenger */
    case object Survived extends Label

    /** Represents the death of the passenger */
    case object Died extends Label
  }

  /** The gender of a passenger.  This is always known. */
  sealed trait Gender
  object Gender {

    /** Represents a male passenger */
    case object Male extends Gender

    /** Represents a female passenger */
    case object Female extends Gender
  }

  /**
    * The age bracket of a passenger.
    *
    * Bracketing continuous values reduces the cardinality of possible inputs, which in
    * turn reduces the complexity of any hypotheses.
    * 
    * The age is binned into 3 brackets following manual data analysis.  A more thorough
    * bracketing could be acheived using clustering techniques, such as k means
    * clustering, or entropy splitting.
    * 
    * The age of the passenger is not always present.  For absent values, the age
    * could be filled with the median age of the sample.
    */
  trait Age

  object Age {

    case object Child extends Age

    case object Adult extends Age

    case object Elder extends Age
  }

  /**
    * The ticket class of a passenger.  This is always present.
    */
  trait TicketClass

  object TicketClass {

    case object First extends TicketClass

    case object Second extends TicketClass

    case object Third extends TicketClass
  }

  /**
    * The family size bracket of the passenger.
    * This can be estimated by counting the number of siblings, spouses, parents and
    * children travelling with them.
    *
    * Passengers with larger families are less likely
    * to survive, as they spend more time looking for and organizing their parents or
    * children.  In some cases, families on the Titanic chose to stay onboard the ship
    * as a group, as they were reluctant to leave alone.
    *
    * As with age, the bracketing here is based on manual data analysis.
    */
  trait FamilySize

  object FamilySize {

    case object Single extends FamilySize

    case object Small extends FamilySize

    case object Large extends FamilySize
    
  }

  trait Port

  object Port {

    case object Queenstown extends Port

    case object Southhampton extends Port

    case object Cherbourg extends Port
  }

  trait HasCabin

  object HasCabin {

    case object Yes extends HasCabin

    case object No extends HasCabin
  }


  case class Feature[E](
    name: String,
    f: E => String
  ) {
    def apply(e: E): String = f(e)
  }

  case class TrainingExample[E](
    example: E,
    label: Label
  )

  /**
    * The features used in creating the decision tree.
    *
    * These features have been chosen after a brief manual analysis of the data.
    * Techniques like principle component analysis can choose features more rigorously.
    *
    * The age and family size brackets are also arbitrary, based on manual analysis.
    * These can be better chosen using clustering techniques.
    */
  case class Example(
    gender: Gender,
    age: Age,
    ticketClass: TicketClass,
    familySize: FamilySize,
    hasCabin: HasCabin,
    port: Port
  )

  val genderFeature = Feature[Example]("gender", _.gender.toString)
  val ageFeature = Feature[Example]("age", _.age.toString)
  val ticketClassFeature = Feature[Example]("ticketClass", _.ticketClass.toString)
  val familySizeFeature = Feature[Example]("familySize", _.familySize.toString)
  val hasCabinFeature = Feature[Example]("hasCabin", _.hasCabin.toString)
  val portFeature = Feature[Example]("port", _.port.toString)

  val features = List(
    genderFeature,
    ageFeature,
    ticketClassFeature,
    familySizeFeature,
    hasCabinFeature,
    portFeature
  )
}



