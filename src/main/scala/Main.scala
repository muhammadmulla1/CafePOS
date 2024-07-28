import scala.util.Random
import java.time.LocalTime

object Main {

  import Models._

  val menu: List[CafeMenu] = List(
    CafeMenu("cake", 1.50, Category.Food, Temperature.Cold, premium = true),
    CafeMenu("espresso", 2.00, Category.Drink, Temperature.Hot, premium = true),
    CafeMenu("sandwich", 3.50, Category.Food, Temperature.Cold, premium = false),
    CafeMenu("iced coffee", 2.50, Category.Drink, Temperature.Cold, premium = true),
    CafeMenu("muffin", 1.75, Category.Food, Temperature.Cold, premium = false),
    CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false),
    CafeMenu("latte", 3.00, Category.Drink, Temperature.Hot, premium = true),
    CafeMenu("bagel", 2.00, Category.Food, Temperature.Cold, premium = false),
    CafeMenu("hot chocolate", 2.75, Category.Drink, Temperature.Hot, premium = true),
    CafeMenu("smoothie", 3.50, Category.Drink, Temperature.Cold, premium = false)
  )


  val currencyExchange: Map[Currency.Value, Double] = Map(
    Currency.GBP -> 1.0,
    Currency.EUR -> 1.19,
    Currency.USD -> 1.29,
    Currency.YEN -> 1.976
  )

  // Convert an amount from GBP to another currency
  def applyCurrencyConversion(amount: Double, currency: Currency.Value): Either[String, Double] = {
    currencyExchange.get(currency) match {
      case None => Left(s"Unsupported currency: $currency") // Return an error message for unsupported currencies
      case Some(rate) => Right(amount * rate) // Convert using the exchange rate
    }
  }

  // Add a premium item to the menu if not already present
  def addPremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    if (!menu.exists(_.name == specialItem.name)) menu :+ specialItem
    else menu
  }

  // Remove a premium item from the menu if present
  def removePremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    if (menu.exists(_.name == specialItem.name)) menu.filterNot(_.name == specialItem.name)
    else menu
  }

  // Create a random order of a specified number of items
  def createRandomOrder(menu: List[CafeMenu], numberOfItems: Int): List[CafeMenu] = {
    Random.shuffle(menu).take(numberOfItems.min(menu.size))
  }

  // Calculate the total bill of an order
  def calculateTotal(order: List[CafeMenu]): Double = order.map(_.price).sum

  // Calculate the service charge based on the order details
  def calculateServiceCharge(order: List[CafeMenu]): Double = {
    if (order.isEmpty) return 0.0 // No charge for empty orders

    val total = calculateTotal(order) // Calculate total bill
    val hasHotDrinks = order.exists(item => item.temperature == Temperature.Hot && item.category == Category.Drink)
    val hasColdFood = order.exists(item => item.temperature == Temperature.Cold && item.category == Category.Food)
    val hasHotFood = order.exists(item => item.temperature == Temperature.Hot && item.category == Category.Food)
    val hasPremiumSpecial = order.exists(_.premium)

    val hotFoodCharge = if (hasHotFood) math.min(total * 0.20, 20.0) else 0.0
    val premiumSpecialCharge = if (hasPremiumSpecial) math.min(total * 0.25, 40.0) else 0.0
    val otherCharge = if (hasHotDrinks || hasColdFood) total * 0.10 else 0.0

    math.max(hotFoodCharge, math.max(premiumSpecialCharge, otherCharge)) // Mmx charge applied
  }

  // final charge including service and custom charges
  def calculateFinalCharge(order: List[CafeMenu], customServiceCharge: Option[Double] = None, additionalCharge: Boolean = false): Double = {
    val total = calculateTotal(order)
    val serviceCharge = calculateServiceCharge(order)
    val finalServiceCharge = customServiceCharge match {
      case Some(charge) if additionalCharge => serviceCharge + charge // add custom charge if wanted
      case Some(charge) => charge // Use custom charge only
      case None => serviceCharge // Use calculated service charge
    }
    total + finalServiceCharge
  }

  //  eligibility fheck foor loyalty cards
  def checkLoyaltyCardEligibility(customer: Customer, loyaltyCard: String): Boolean = {
    val totalPurchaseHistory = customer.purchaseHistory.map(_.price).sum
    val hasRequiredAge = customer.age >= 18

    loyaltyCard.toLowerCase match {
      case "drinks" =>
        hasRequiredAge &&
          customer.purchaseHistory.count(_.category == Category.Drink) >= 5 &&
          customer.drinksLoyaltyCard.isEmpty //  eligible for drinks loyalty card

      case "discount" =>
        hasRequiredAge &&
          customer.purchaseHistory.size >= 5 &&
          totalPurchaseHistory >= 150 &&
          customer.discountLoyaltyCard.isEmpty // eligible for discount loyalty card

      case _ => false
    }
  }

  // Update drinks loyalty card
  def updateDrinksLoyaltyCard(customer: Customer): Customer = {
    customer.drinksLoyaltyCard match {
      case Some(card) if card.stamps < 9 =>
        val updatedCard = card.copy(stamps = card.stamps + 1) // Add a stamp if not full
        customer.copy(drinksLoyaltyCard = Some(updatedCard))
      case Some(card) if card.stamps == 10 =>
        val updatedCard = card.copy(stamps = 0) // Reset stamps if full
        customer.copy(drinksLoyaltyCard = Some(updatedCard))
      case None => customer // Return unchanged customer if no card
    }
  }

  //Update customer discount loyalty card (stars)
  def updateDiscountLoyaltyCard(customer: Customer, orderTotal: Double): Customer = {
    if (orderTotal > 20) {
      customer.discountLoyaltyCard match {
        case Some(card) if card.stars < 8 =>
          val updatedCard = card.copy(stars = card.stars + 1) // add a star if not at cap
          customer.copy(discountLoyaltyCard = Some(updatedCard))
        case None => customer
      }
    } else {
      customer // doesn't qualify for a star
    }
  }


  def isHappyHour(currentTime: LocalTime = LocalTime.now(), start: LocalTime = LocalTime.of(18, 0), end: LocalTime = LocalTime.of(19, 0)): Boolean = {
    currentTime.isAfter(start) && currentTime.isBefore(end)
  }

  // Apply discount based on discount loyalty card stars and happy hour
  def applyDiscount(customer: Customer, currentTime: LocalTime = LocalTime.now()): Double = {
    val happyHour = isHappyHour(currentTime)
    val (foodItems, drinkItems) = customer.purchaseHistory.partition(_.category == Category.Food) // split purchase history into fooditems and drinkitems(returns a tuple)
    val nonPremiumFoodItems = foodItems.filterNot(_.premium) // Exclude premium food items

    val discountedFoodTotal = customer.discountLoyaltyCard match {
      case Some(card) if card.stars == 8 =>
        nonPremiumFoodItems.map(_.price).sum * 0.84 //Apply 16 discount to nonpremium food
      case Some(card) =>
        nonPremiumFoodItems.map(_.price).sum * (1 - card.stars * 0.02) // spply discount based on stars to non-premium food
      case None => nonPremiumFoodItems.map(_.price).sum //no discount if no card
    }

    val finalDrinkTotal = if (happyHour) drinkItems.map(_.price).sum / 2 else drinkItems.map(_.price).sum // Apply half price for drinks during happy hour

    discountedFoodTotal + finalDrinkTotal // Return total after applying discounts
  }

  // check if staff is eligible for a discount
  def eligibleForDiscount(staff: Staff): Either[String, Boolean] = {
    if (staff.monthsWorked > staff.age) Left("Illegal: Months worked is higher than the age")
    else if (staff.monthsWorked >= 6 && staff.age >= 18) Right(true) //  if criteria met the eligable
    else Right(false)
  }

  // Apply staff discount to total bill
  def applyStaffDiscount(staff: Staff, totalBill: Double): Either[String, Double] = {
    eligibleForDiscount(staff) match {
      case Right(true) => Right(totalBill * 0.9) // 10% discount if eligible
      case Right(false) => Right(totalBill) //No discount
      case Left(error) => Left(error)
    }
  }
}
