//class CGTCalculator {
//  private val cgtLowerRate: Double = 0.1
//  private val cgtHigherRate: Double = 0.2
//
//  private val cgtAllowance: Double = 3000
//  private val capitalGainsWithinThreshold: Double = 2000
//  private val capitalGains: Double = 20000
//  private val annualIncomeThreshold: Double = 50270
//
//  def calculateCapitalGains (income: Double): Double = {
//    if (income <= 0) 404
//    else if (income <= annualIncomeThreshold && income >= 3000) (capitalGains - cgtAllowance) * cgtLowerRate
//    else if (income >= annualIncomeThreshold) (capitalGains - cgtAllowance) * cgtHigherRate
//    else if (capitalGainsWithinThreshold <= cgtAllowance) 0
//    else 404
//  }
//}

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