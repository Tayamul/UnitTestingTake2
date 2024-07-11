import org.scalatest.wordspec.AnyWordSpec

class CGTCalculatorSpec extends AnyWordSpec {
  private val cgtLowerRate: Double = 0.1
  private val cgtHigherRate: Double = 0.2

  private val cgtAllowance: Double = 3000
  private val annualIncomeThreshold: Double = 50270

  val cgtCalculator: CGTCalculator = new CGTCalculator

  "CGTCalculator.calculateCGT" should {
    "return the total amount of capital gains tax to pay" when {
      "the overall annual income is below £50,270 AND the capital gains is £20,000" in {
        val income: Double = 30000
        val capitalGains: Double = 20000
        val result = cgtCalculator.calculateCGT(income, capitalGains)
        assert(result == (capitalGains - cgtAllowance) * cgtLowerRate)
      }
    }
    "return the total amount of capital gains tax to pay" when {
      "the overall annual income is above £50,270 AND the capital gains is £20,000" in {
        val income: Double = 75000
        val capitalGains: Double = 20000
        val result = cgtCalculator.calculateCGT(income, capitalGains)
        assert(result == (capitalGains - cgtAllowance) * cgtHigherRate)
      }
    }
    "return the total amount of capital gains tax to pay" when {
      s"the capital gains from shares is within the Capital Gains Tax Allowance rate of £${cgtAllowance.toInt}" in {
        val income: Double = 75000
        val capitalGains: Double = 2000
        val result = cgtCalculator.calculateCGT(income, capitalGains)
        assert(result == 0)
      }
    }
  }
}