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
        val income: Double = 5000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * personalAllowanceRate)
      }
    }
    "return the total amount of tax to pay" when {
      "the income is below the basic rate but above the personal allowance" in {
        val income: Double = 25000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == ((income - personalAllowance) * basicRate))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is in the higher tax bracket" in {
        val income: Double = 60000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == (((basicRateLimit - personalAllowance) * basicRate) + ((income - basicRateLimit) * higherRate)))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is in the additional tax bracket" in {
        val income: Double = 150000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == (((basicRateLimit - personalAllowance) * basicRate) + ((higherRateLimit - basicRateLimit) * higherRate) + ((income - higherRateLimit) * additionalRate)))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is exactly at the personal allowance" in {
        val income: Double = 10000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == income * personalAllowanceRate)
      }
    }
    "return the total amount of tax to pay" when {
      "the income is exactly at the basic rate limit" in {
        val income: Double = 50000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == ((income - personalAllowance) * basicRate))
      }
    }
    "return the total amount of tax to pay" when {
      "the income is exactly at the higher tax limit" in {
        val income: Double = 125000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == (((basicRateLimit - personalAllowance) * basicRate) + ((income - basicRateLimit) * higherRate)))
      }
    }
    "return a 404 error message" when {
      "the input is 0" in {
        val income: Double = 0
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == 404)
      }
    }
    "return a 404 error message" when {
      "the input is a negative integer" in {
        val income: Double = -65000
        val result: Double = taxCalculator.calculateTax(income)
        assert(result == 404)
      }
    }
  }

  // isHigherRateTaxpayer method!
  "TaxCalculator.isHigherRateTaxpayer" should {
    "return a true value" when {
      "the employee pays a higher tax rate" in {
        val income: Double = 75000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a false value" when {
      "the employee does not pay a higher tax rate" in {
        val income: Double = 30000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
    "return a true value" when {
      "the employee's pay is exactly at the higher rate limit" in {
        val income: Double = 125000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a true value" when {
      "the employee's pay is just above the basic rate limit" in {
        val income: Double = 50001
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a true value" when {
      "the employee's pay is just below the additional rate limit" in {
        val income: Double = 124999
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(result)
      }
    }
    "return a false value" when {
      "the employee pays an additional tax rate" in {
        val income: Double = 195000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
    "return a false value" when {
      "the input is 0" in {
        val income: Double = 0
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
    "return a false value" when {
      "the input is a negative integer" in {
        val income: Double = -50000
        val result: Boolean = taxCalculator.isHigherRateTaxpayer(income)
        assert(!result)
      }
    }
  }

  // formattedCurrentTaxAllowance method!
  "TaxCalculator.formattedCurrentTaxAllowance" should {
    "return the correct tax band for an income below the personal allowance" in {
      val income: Double = 8500
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £0 - £$personalAllowance.")
    }
    "return the correct tax band for an income in the basic rate band" in {
      val income: Double = 35000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${personalAllowance + 1} - £$basicRateLimit.")
    }
    "return the correct tax band for an income in the higher rate band" in {
      val income: Double = 90000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${basicRateLimit + 1} - £$higherRateLimit.")
    }
    "return the correct tax band for an income in the additional rate band" in {
      val income: Double = 210000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: Above £$higherRateLimit.")
    }
    "return the correct tax band for an income exactly at the personal allowance rate" in {
      val income: Double = 10000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £0 - £$personalAllowance.")
    }
    "return the correct tax band for an income exactly at the basic rate" in {
      val income: Double = 50000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${personalAllowance + 1} - £$basicRateLimit.")
    }
    "return the correct tax band for an income exactly at the higher rate" in {
      val income: Double = 125000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${basicRateLimit + 1} - £$higherRateLimit.")
    }
    "return the correct tax band for an income just above the higher rate band" in {
      val income: Double = 125001
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: Above £$higherRateLimit.")
    }
    "return the correct tax band for an income just below the higher rate band" in {
      val income: Double = 124999
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${basicRateLimit + 1} - £$higherRateLimit.")
    }
    "return the correct tax band for an income just above the basic rate band" in {
      val income: Double = 50001
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${basicRateLimit + 1} - £$higherRateLimit.")
    }
    "return the correct tax band for an income just below the basic rate band" in {
      val income: Double = 49999
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${personalAllowance + 1} - £$basicRateLimit.")
    }
    "return the correct tax band for an income just above the personal allowance" in {
      val income: Double = 10001
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: £${personalAllowance + 1} - £$basicRateLimit.")
    }
    "return the correct tax band for a large amount of income" in {
      val income: Double = 1250000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == s"Income limit for your current tax band: Above £$higherRateLimit.")
    }
    "return an appropriate error message if the input is 0" in {
      val income: Double = 0
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == "Must be an error! No one works for free.")
    }
    "return an appropriate error message for a negative income" in {
      val income: Double = -25000
      val result: String = taxCalculator.formattedCurrentTaxAllowance(income)
      assert(result == "Invalid input: Salary must be a non-negative number.")
    }
  }
}
