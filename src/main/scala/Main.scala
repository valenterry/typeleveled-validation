import cats.implicits._
import cats.data.ValidatedNel
import Validatable._

case class UserForm(firstName: FirstName, lastName: LastName, address: Address)
case class FirstName(value: String) extends AnyVal
case class LastName(value: String) extends AnyVal
case class Address(street: Street, city: City, zipCode: ZipCode, country: Country)
case class Street(name: StreetName, number: StreetNumber)
case class StreetName(value: String) extends AnyVal
case class StreetNumber(value: String) extends AnyVal
case class City(value: String) extends AnyVal
case class ZipCode(value: Int) extends AnyVal
case class Country(value: String) extends AnyVal

object ValidatableInstances {
  //Some examples of already implemented validation
  implicit val firstNameValidatable = Validatable.from[FirstName] { name =>
    if (name.value.size > 30) s"Your first name is too long (${name.value.size} chars). Please change it to < 30".invalidNel else Validatable.valid
  }

  implicit val lastNameValidatable = Validatable.from[LastName] { name =>
    if (name.value.capitalize != name.value) s"Your last name doesn't start with a capital letter? Really?".invalidNel else Validatable.valid
  }

  implicit val countryValidation = Validatable.from[Country] { country =>
    country.value match {
      case "Germany" | "Japan" | "Iceland" => Validatable.valid
      case other: String                   => s"We don't really like $other. Please move to a nicer country!".invalidNel
    }
  }

  // TODO: implement validation...
  //zipcode is valid between 0 and 99999
  implicit val zipCodeValidatable: Validatable[ZipCode] = ???

  //city is always valid
  implicit val cityValidatable: Validatable[City] = ???

  //Streetnumber and name must not be empty
  implicit val streetNumberValidatable: Validatable[StreetNumber] = ???
  implicit val streetNameValidatable: Validatable[StreetName] = ???

  //Street, adress and whole userform are valid if all of their fields are valid
  //They contain all the errors of their inner fields
  implicit val streetValidatable: Validatable[Street] = ???
  implicit val addressValidatable: Validatable[Address] = ???
  implicit val userFormValidatable: Validatable[UserForm] = ???
}

trait Validatable[A] {
  def validate(a: A): ValidationResult
}

object Validatable {
  type Error            = String
  type ValidationResult = ValidatedNel[Error, Unit]

  //Helpers
  val valid: ValidationResult        = ().validNel
  def alwaysValid[A]: Validatable[A] = Validatable.from(a => ().validNel)
  def from[A](validateFunction: A => ValidationResult): Validatable[A] = (a: A) => validateFunction(a)

  //Syntax
  implicit class ValidatableOps[A: Validatable](private val a: A) {
    def validate = implicitly[Validatable[A]].validate(a)
  }
}