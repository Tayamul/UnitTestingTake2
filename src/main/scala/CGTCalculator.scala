class CGTCalculator {

  private val cgtLowerRate: Double = 0.1
  private val cgtHigherRate: Double = 0.2

  private val cgtAllowance: Double = 3000
  private val annualIncomeThreshold: Double = 50270

  def calculateCGT (income: Double, capitalGains: Double): Double = {
    if (capitalGains <= 3000 && capitalGains > 0) 0
    else if (income <= annualIncomeThreshold) (capitalGains - cgtAllowance) * cgtLowerRate
    else if (income >= annualIncomeThreshold) (capitalGains - cgtAllowance) * cgtHigherRate
    else 404
  }
}