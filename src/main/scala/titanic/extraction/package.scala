package titanic

import titanic.model._
import titanic.hypothesis.Hypothesis

package object extraction {


  /** Extracts the gender from the raw data.  This is always present. */
  def gender(row: TestDataRow): Gender = {
    row.gender.toLowerCase match {
      case "male" => Gender.Male
      case "female" => Gender.Female
    }
  }

  /**
    * Extracts the age bracket from a row using arbitrary age bounds.
    *
    * If no age is present, the passenger is assumed to be an adult.
    */
  def age(row: TestDataRow): Age =
    if(row.age.exists(_ < 15)) Age.Child
    else if(row.age.exists(_ > 50)) Age.Elder
    else Age.Adult

  /**
    * Extracts the ticket class of a passenger.  This is always present.
    */
  def ticketClass(row: TestDataRow): TicketClass = row.pclass match {
    case 1 => TicketClass.First
    case 2 => TicketClass.Second
    case _ => TicketClass.Third
  }

  /**
    * Extracts the family size of the passenger.
    *
    * This is the number of siblings, spouses, parents and children travelling with the
    * passenger.
    * Families travelling alone have a family size of 0.
    */ 
  def familySize(row: TestDataRow): FamilySize = {
    val size = row.sibsp + row.parch
    if(size == 0) FamilySize.Single
    else if(size == 1) FamilySize.Small
    else FamilySize.Large
  }

  /**
    * Extracts the port that the passenger embarked from.
    *
    * If no port is present, it is assumed that they came from Southhampton.  This is
    * the most common embarkation port.
    * 
    */
  def port(row: TestDataRow): Port = row.embarked match {
    case "Q" => Port.Queenstown
    case "C" => Port.Cherbourg
    case _ => Port.Southhampton
  }

  /**
    * Extracts whether or not the passenger has a cabin.
    */
  def hasCabin(row: TestDataRow): HasCabin =
    if(row.cabin.trim.isEmpty) HasCabin.No
    else HasCabin.Yes
}
