import org.scalatest.FunSpec

class ValidationTests extends FunSpec {
  import Validatable.ValidatableOps
  import ValidatableInstances._

  describe("validation works for") {
    it("zipcode") {
      assert(ZipCode(23456).validate.isValid)
      assert(ZipCode(-50).validate.isInvalid)
      assert(ZipCode(100000).validate.isInvalid)
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
}
