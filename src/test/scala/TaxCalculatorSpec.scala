import org.scalatest.matchers.should.Matchers._
import org.scalatest.wordspec.AnyWordSpec

class TaxCalculatorSpec extends AnyWordSpec {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  val taxCalculator: TaxCalculator = new TaxCalculator

  // I've done the first test for you!
  "TaxCalculator.calculateTax" should {
    "return the total amount of tax to pay" when {
      "the income is below the personal tax limit" in {
        val income = 5000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * personalAllowanceRate)
      }
    }
    "return the total amount of tax to pay" when {
      "the income is below the basic rate but above the personal allowance" in {
        val income = 25000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == ((income - personalAllowance) * basicRate))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is in the higher tax bracket" in {
        val income = 60000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == (((basicRateLimit - personalAllowance) * basicRate) + ((income - basicRateLimit) * higherRate)))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is in the additional tax bracket" in {
        val income = 150000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == (((basicRateLimit - personalAllowance) * basicRate) + ((higherRateLimit - basicRateLimit) * higherRate) + ((income - higherRateLimit) * additionalRate)))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is exactly at the personal allowance" in {
        val income = 10000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * personalAllowanceRate)
      }
    }
    "return the total amount of tax to pay" when {
      "the income is exactly at the basic rate limit" in {
        val income = 50000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == ((income - personalAllowance) * basicRate))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is exactly at the higher tax limit" in {
        val income = 125000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == (((basicRateLimit - personalAllowance) * basicRate) + ((income - basicRateLimit) * higherRate)))
      }
    }
    "return a 404 error message" when {
      "the input is 0" in {
        val result: Double = taxCalculator.calculateTax(0)
        assert(result == 404)
      }
    }
    "return a 404 error message" when {
      "the input is a negative integer" in {
        val result: Double = taxCalculator.calculateTax(-65000)
        assert(result == 404)
      }
    }
  }

  "TaxCalculator.isHigherRateTaxpayer" should {
    "return a true value" when {
      "the employee pays a higher tax rate" in {
        val income = 75000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a false value" when {
      "the employee does not pay a higher tax rate" in {
        val income = 30000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
    "return a true value" when {
      "the employee's pay is exactly at the higher rate limit" in {
        val income = 125000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a true value" when {
      "the employee's pay is just above the basic rate limit" in {
        val income = 50001
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a true value" when {
      "the employee's pay is just below the additional rate limit" in {
        val income = 124999
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a false value" when {
      "the employee pays an additional tax rate" in {
        val income = 195000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
    "return a false value" when {
      "the input is 0" in {
        val income = 0
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
    "return a false value" when {
      "the input is a negative integer" in {
        val income = -50000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
  }
}
