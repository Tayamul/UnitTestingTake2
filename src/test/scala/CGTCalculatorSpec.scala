import org.scalatest.wordspec.AnyWordSpec

//class CGTCalculatorSpec extends AnyWordSpec {
//  private val cgtLowerRate: Double = 0.1
//  private val cgtHigherRate: Double = 0.2
//
//  private val cgtAllowance: Double = 3000
//  private val capitalGains: Double = 20000
//  private val annualIncomeThreshold: Double = 50270
//
//  val cgtCalculator: CGTCalculator = new CGTCalculator
//
//  "CGTCalculator.calculateCapitalGains" should {
//    "return the total amount of capital gains tax to pay" when {
//      s"the overall annual income is below £${annualIncomeThreshold.toInt}" in {
//        val income: Double = 30000
//        val result: Double = cgtCalculator.calculateCapitalGains(income)
//        assert(result == (capitalGains - cgtAllowance) * cgtLowerRate)
//      }
//    }
//    "return the total amount of capital gains tax to pay" when {
//      s"the overall annual income is above £${annualIncomeThreshold.toInt}" in {
//        val income: Double = 60000
//        val result: Double = cgtCalculator.calculateCapitalGains(income)
//        assert(result == (capitalGains - cgtAllowance) * cgtHigherRate)
//      }
//    }
//    "return the total amount of capital gains tax to pay" when {
//      s"the capital gains from shares is within the Capital Gains Tax Allowance rate of £${cgtAllowance.toInt}" in {
//        val income: Double = 2000
//        val result: Double = cgtCalculator.calculateCapitalGains(income)
//        assert(result == 0)
//      }
//    }
//    "return a 404 error message" when {
//      "the income is 0" in {
//        val income: Double = 0
//        val result: Double = cgtCalculator.calculateCapitalGains(income)
//        assert(result == 404)
//      }
//    }
//    "return a 404 error message" when {
//      "the income is non-negative integer" in {
//        val income: Double = -30000
//        val result: Double = cgtCalculator.calculateCapitalGains(income)
//        assert(result == 404)
//      }
//    }
//
//  }
//}

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