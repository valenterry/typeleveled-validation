import cats.data.NonEmptyList
import cats.data.Validated.Invalid
import org.scalatest.{FunSpec, Matchers}

class ValidationTests extends FunSpec with Matchers {
  import Validatable.ValidatableOps
  import ValidatableInstances._

  describe("validation works for") {
    it("zipcode") {
      assert(ZipCode(23456).validate.isValid)
      assert(ZipCode(-50).validate.isInvalid)
    }
    it("city") {
      assert(City("Hamburg").validate.isValid)
      assert(City("").validate.isValid)
    }
    it("streetname") {
      assert(StreetName("Koenigsallee").validate.isValid)
      assert(StreetName("").validate.isInvalid)
    }
    it("streetnumber") {
      assert(StreetNumber("15b").validate.isValid)
      assert(StreetNumber("").validate.isInvalid)
    }
    it("street") {
      assert(Street(StreetName("Koenigsallee"), StreetNumber("15")).validate.isValid)
      assert(Street(StreetName(""), StreetNumber("15")).validate.isInvalid)
      assert(Street(StreetName("Koenigsallee"), StreetNumber("")).validate.isInvalid)
      Street(StreetName(""), StreetNumber("")).validate.fold(
        errors => assert(errors.size == 2),
        Unit => assert(false)
      )
    }
    it("address") {
      val validStreet   = Street(StreetName("Koenigsallee"), StreetNumber("15"))
      val invalidStreet = Street(StreetName(""), StreetNumber(""))
      assert(Address(validStreet, City("Hamburg"), ZipCode(12345), Country("Germany")).validate.isValid)
      Address(invalidStreet, City(""), ZipCode(-5555), Country("Ireland")).validate.fold(
        errors => assert(errors.size == 4),
        Unit => assert(false)
      )
    }
    it("userForm") {
      val validAddress   = Address(Street(StreetName("Koenigsallee"), StreetNumber("15")), City("Hamburg"), ZipCode(12345), Country("Germany"))
      val invalidAddress = Address(Street(StreetName(""), StreetNumber("")), City("Hamburg"), ZipCode(-5555), Country("Ireland"))
      assert(UserForm(FirstName("Jasmin"), LastName("Laba"), validAddress).validate.isValid)
      UserForm(FirstName("Jasminxxxxxxxxxxxxxxxxxxxxxxxxxx"), LastName("laba"), invalidAddress).validate.fold(
        errors => assert(errors.size == 6),
        Unit => assert(false)
      )
    }
  }

  describe("validation contains correct error pathes for") {
    it("zipcode") {
      ZipCode(-50).validate should matchPattern {
        case Invalid(NonEmptyList((Nil, _), Nil)) =>
      }
    }
    it("streetname") {
      StreetName("").validate should matchPattern {
        case Invalid(NonEmptyList((Nil, _), Nil)) =>
      }
    }
    it("streetnumber") {
      StreetNumber("").validate should matchPattern {
        case Invalid(NonEmptyList((Nil, _), Nil)) =>
      }
    }
    it("street") {
      Street(StreetName(""), StreetNumber("")).validate should matchPattern {
        case Invalid(NonEmptyList(("name" :: Nil, _), ("number" :: Nil, _) :: Nil)) =>
      }
    }
    it("address") {
      Address(Street(StreetName(""), StreetNumber("")), City(""), ZipCode(-5555), Country("Ireland")).validate should matchPattern {
        case Invalid(NonEmptyList(("street" :: "name" :: Nil, _), ("street" :: "number" :: Nil, _) :: ("zipCode" :: Nil, _) :: ("country" :: Nil, _) :: Nil)) =>
      }
    }
    it("userForm") {
      UserForm(FirstName("Jasminxxxxxxxxxxxxxxxxxxxxxxxxxx"), LastName("laba"), Address(Street(StreetName(""), StreetNumber("")), City("Hamburg"), ZipCode(-5555), Country("Ireland"))).validate should matchPattern {
        case Invalid(NonEmptyList(("firstName" :: Nil, _), ("lastName" :: Nil, _) :: ("address" :: "street" :: "name" :: Nil, _) :: ("address" :: "street" :: "number" :: Nil, _) :: ("address" :: "zipCode" :: Nil, _) :: ("address" :: "country" :: Nil, _) :: Nil)) =>
      }
    }
  }
}
