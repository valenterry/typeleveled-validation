import shapeless._
import cats._
import cats.data._
import cats.implicits._
import cats.data.ValidatedNel
import Validatable._
import cats.data.Validated.{Invalid, Valid}
import shapeless.LabelledGeneric.Aux
import shapeless.labelled.FieldType
import shapeless.ops.hlist._

case class UserForm(firstName: FirstName, lastName: LastName, address: Address)
case class FirstName(value: String) extends AnyVal
case class LastName(value: String)  extends AnyVal
case class Address(street: Street, city: City, zipCode: ZipCode, country: Country)
case class Street(name: StreetName, number: StreetNumber)
case class StreetName(value: String)   extends AnyVal
case class StreetNumber(value: String) extends AnyVal
case class City(value: String)         extends AnyVal
case class ZipCode(value: Int)         extends AnyVal
case class Country(value: String)      extends AnyVal

object ValidatableInstances {
  //Some examples of already implemented validation
  implicit val firstNameValidatable = Validatable.from[FirstName] { name =>
    if (name.value.size > 30) (Nil -> s"Your first name is too long (${name.value.size} chars). Please change it to < 30").invalidNel else Validatable.valid
  }

  implicit val lastNameValidatable = Validatable.from[LastName] { name =>
    if (name.value.capitalize != name.value) (Nil -> s"Your last name doesn't start with a capital letter? Really?").invalidNel else Validatable.valid
  }

  implicit val countryValidation = Validatable.from[Country] { country =>
    country.value match {
      case "Germany" | "Japan" | "Iceland" => Validatable.valid
      case other: String                   => (Nil -> s"We don't really like $other. Please move to a nicer country!").invalidNel
    }
  }

  //zipcode is valid between 0 and 99999
  implicit val zipCodeValidatable: Validatable[ZipCode] = Validatable.from[ZipCode] { zipCode =>
    if (zipCode.value < 0 || zipCode.value > 99999) (Nil -> "Zipcode out of range").invalidNel else Validatable.valid
  }

  //city is always valid
  implicit val cityValidatable: Validatable[City] = Validatable.alwaysValid[City]

  //Streetnumber and name must not be empty
  implicit val streetNumberValidatable: Validatable[StreetNumber] = Validatable.from[StreetNumber] { num =>
    if (num.value.isEmpty) (Nil -> "Street number must not be empty").invalidNel else Validatable.valid
  }
  implicit val streetNameValidatable: Validatable[StreetName] = Validatable.from[StreetName] { name =>
    if (name.value.isEmpty) (Nil -> "Street name must not be empty").invalidNel else Validatable.valid
  }

  //Street, adress and whole userform are valid if all of their fields are valid
  //They contain all the errors of their inner fields, with the fields errors having the field name in the list
  //These are generated at compiletime
  implicit val streetValidatable: Validatable[Street]     = Validatable.from[Street](generateValidationResult(_))
  implicit val addressValidatable: Validatable[Address]   = Validatable.from[Address](generateValidationResult(_))
  implicit val userFormValidatable: Validatable[UserForm] = Validatable.from[UserForm](generateValidationResult(_))
}

trait Validatable[A] {
  def validate(a: A): ValidationResult
}

object Validatable {
  //Error type now contains the path to the field where the error occurred
  //When validating a user form and the addresses street number is invalid, the error is: (List('address', 'street', 'number'), "street number is invalid")
  //When validating country for itself and itself is invalid (not one of it's fields, because it has none / is a value class), the error is: (Nil, "country is invalid")
  type Error            = (List[String], String)
  type ValidationResult = ValidatedNel[Error, Unit]

  //Helpers
  val valid: ValidationResult                                          = ().validNel
  def alwaysValid[A]: Validatable[A]                                   = Validatable.from(a => ().validNel)
  def from[A](validateFunction: A => ValidationResult): Validatable[A] = (a: A) => validateFunction(a)
  def prependField(fieldName: String, result: ValidationResult) =
    result.leftMap(errors =>
      errors.map {
        case (fieldNames, message) => (fieldName :: fieldNames, message)
    })

  //Syntax
  implicit class ValidatableOps[A: Validatable](private val a: A) {
    def validate = implicitly[Validatable[A]].validate(a)
  }

  //Helpers to generate ValidationResult from a case class instance for which each of its fields has an Validatable instance
  object CombineToValidationResult extends Poly2 {
    implicit def combine[AttrName <: Symbol, AttrType: Validatable](
        implicit fieldNameWit: Witness.Aux[AttrName]): Case.Aux[ValidationResult, FieldType[AttrName, AttrType], ValidationResult] =
      at { (startResult: ValidationResult, nextField: FieldType[AttrName, AttrType]) =>
        startResult |+| Validatable.prependField(fieldNameWit.value.name, the[Validatable[AttrType]].validate(nextField))
      }
  }

  def generateValidationResult[InputType, InputLG <: HList, CombinedValidationResult](obj: InputType)(
      implicit
      labelledGeneric: LabelledGeneric.Aux[InputType, InputLG],
      leftFolder: LeftFolder.Aux[InputLG, ValidationResult, CombineToValidationResult.type, CombinedValidationResult]): CombinedValidationResult =
    labelledGeneric.to(obj).foldLeft(Validatable.valid)(CombineToValidationResult)
}
