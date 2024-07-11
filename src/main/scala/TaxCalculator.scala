class TaxCalculator {

  // Tax bands (simplified to make testing a bit easier)
  private val personalAllowance: Int = 10000
  private val basicRateLimit: Int = 50000
  private val higherRateLimit: Int = 125000

  // Tax rates
  private val personalAllowanceRate: Double = 0
  private val basicRate: Double = 0.2
  private val higherRate: Double = 0.4
  private val additionalRate: Double = 0.45

  // A method to calculate the total amount of tax to be paid, returned as a double
  def calculateTax(income: Double): Double = {
    if (income <= 0) 404
    else if (income <= personalAllowance) income * personalAllowanceRate
    else if (income <= basicRateLimit) (income - personalAllowance) * basicRate
    else if (income <= higherRateLimit) (((basicRateLimit - personalAllowance) * basicRate ) + ((income - basicRateLimit) * higherRate))
    else (((basicRateLimit - personalAllowance) * basicRate) + ((higherRateLimit - basicRateLimit) * higherRate) + ((income - higherRateLimit) * additionalRate))
  }

  // A method which can tell you if someone is a higher rate taxpayer
  def isHigherRateTaxpayer(income: Double): Boolean = {
    income >= basicRateLimit && income <= higherRateLimit
  }

  // A method that will return a string with the income limit of their current tax band.
  // The return will also be formatted, E.g: "£12,500" or "No limit"
  def formattedCurrentTaxAllowance(income: Double): String = {
    if (income == 0) "Must be an error! No one works for free."
    else if (income < 0 ) "Invalid input: Salary must be a non-negative number."
    else if (income <= personalAllowance) s"Income limit for your current tax band: £0 - £$personalAllowance."
    else if (income <= basicRateLimit) s"Income limit for your current tax band: £${personalAllowance + 1} - £$basicRateLimit."
    else if (income <= higherRateLimit) s"Income limit for your current tax band: £${basicRateLimit + 1} - £$higherRateLimit."
    else if (income >= higherRateLimit) s"Income limit for your current tax band: Above £$higherRateLimit."
    else "Something went wrong! Please try again."
  }
}
