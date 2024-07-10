import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  val taxCalculator: TaxCalculator = new TaxCalculator

  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal tax limit" in {
        val result: Double = taxCalculator.calculateTax(5000)

        assert(result == 0)
      }
    }
  }

  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the basic rate, but above the personal allowance" in {
        val result: Double = taxCalculator.calculateTax(25000)
        assert(result == (25000 * 0.2)) // is there a way to refer to the 'basicRate' variable?
      }
    }
  }

  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the employee is on a higher tax bracket" in {
        val result: Double = taxCalculator.calculateTax(100000)
        assert(result == (100000 * 0.4))
      }
    }
  }
}
