import scala.util.Random
import java.time.LocalTime

object Main {

  import Models._


  // List with the cafe menu items
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
  // exchange rate map
  val currencyExchange: Map[Currency.Value, Double] = Map(
    Currency.GBP -> 1.0,
    Currency.EUR -> 1.19,
    Currency.USD -> 1.29,
    Currency.YEN -> 1.976
  )


  // Convert the price of an item GBP -> new currency
  def convertCurrency(price: Double, currency: Currency.Value): Double = {
    price * currencyExchange(currency)
  }
  // apply the currency conversion to a total bill
  def applyCurrencyConversion(totalBill: Double, currency: Currency.Value): Double = {
    convertCurrency(totalBill, currency)
  }
  // add a premium item to the menu
  def addPremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu :+ specialItem
  }

  // Remove premium special from the menu
  def removePremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu.filterNot(_ == specialItem)
  }

  // Create a random order of a specified number of items
  def createRandomOrder(menu: List[CafeMenu], numberOfItems: Int): List[CafeMenu] = {
    Random.shuffle(menu).take(numberOfItems)
  }

  // Calculate service charge for an order
  def calculateServiceCharge(order: List[CafeMenu]): Double = {
    val total = order.map(_.price).sum
    val hasHotDrinks = order.exists(item => item.temperature == Temperature.Hot && item.category == Category.Drink)
    val hasColdFood = order.exists(item => item.temperature == Temperature.Cold && item.category == Category.Food)
    val hasHotFood = order.exists(item => item.temperature == Temperature.Hot && item.category == Category.Food)
    val hasPremiumSpecial = order.exists(_.premium)

    val hotFoodCharge = if (hasHotFood) math.min(total * 0.20, 20.0) else 0.0
    val premiumSpecialCharge = if (hasPremiumSpecial) math.min(total * 0.25, 40.0) else 0.0
    val otherCharge = if (hasHotDrinks || hasColdFood) total * 0.10 else 0.0

    math.max(hotFoodCharge, math.max(premiumSpecialCharge, otherCharge))
  }

  // Calculate total bill
  def calculateTotal(order: List[CafeMenu]): Double = {
    order.map(_.price).sum
  }




  // Calculate final charge, includes service charge and optional custom charges
  def calculateFinalCharge(order: List[CafeMenu], customServiceCharge: Option[Double] = None, additionalCharge: Boolean = false): Double = {
    val total = calculateTotal(order)
    val serviceCharge = calculateServiceCharge(order)
    val finalServiceCharge = customServiceCharge match {
      case Some(charge) if additionalCharge => serviceCharge + charge
      case Some(charge) => charge
      case None => serviceCharge
    }
    total + finalServiceCharge
  }

  def checkLoyaltyCardEligibility(customer: Customer, loyaltyCard: String): Boolean = {
    val totalPurchaseHistory = customer.purchaseHistory.map(_.price).sum
    loyaltyCard.toLowerCase match {
      case "drinks" =>
        customer.age >= 18 &&
          customer.purchaseHistory.count(_.category == Category.Drink) >= 5 &&
          customer.drinksLoyaltyCard.isEmpty

      case "discount" =>
        customer.age >= 18 &&
          customer.purchaseHistory.size >= 5 &&
          totalPurchaseHistory >= 150 &&
          customer.discountLoyaltyCard.isEmpty

      case _ => false
    }
  }



  // update customers drinks loyalty card
  def updateDrinksLoyaltyCard(customer: Customer): Customer = {
    customer.drinksLoyaltyCard match {
      case Some(card) if card.stamps < 9 =>
        val newStamps = card.stamps +1
        val updatedCard = card.copy(stamps = newStamps)
        customer.copy(drinksLoyaltyCard = Some(updatedCard))
      case Some(card) if card.stamps == 10 =>
        val updatedCard = card.copy(stamps = 0)
        customer.copy(drinksLoyaltyCard = Some(updatedCard))
      case Some(card) => customer
      case None => customer
    }
  }
  // update customers discount loyalty card (stars)
  def updateDiscountLoyaltyCard(customer: Customer, orderTotal: Double): Customer = {
    if (orderTotal > 20) {
      customer.discountLoyaltyCard match {
        case Some(card) if card.stars < 8 =>
          val newStars = card.stars + 1
          val updatedCard = card.copy(stars = newStars)
          customer.copy(discountLoyaltyCard = Some(updatedCard))
        case Some(card) =>
          customer // Already has maximum stars
        case None =>
          customer // No discount card
      }
    } else {
      customer // Order doesn't qualify for a star
    }
  }



  def isHappyHour:Boolean = {
    val now = LocalTime.now()
    val start = LocalTime.of(18,0)
    val end   = LocalTime.of(19,0)
    now.isAfter(start) && now.isBefore(end)
  }

  // Apply discount based on discount loyalty card stars
  def applyDiscount(customer: Customer, orderTotal: Double): Double = {
    val happyHour = isHappyHour
    val foodTotal = customer.purchaseHistory.filter(_.category == Category.Food).map(_.price).sum
    val drinksTotal = customer.purchaseHistory.filter(_.category == Category.Drink).map(_.price).sum

    if (happyHour) {
      // During happy hour, drinks are half price, no loyalty discount on drinks
      val happyHourDrinksTotal = drinksTotal / 2
      foodTotal + happyHourDrinksTotal
    } else {
      // Apply loyalty discount to food only
      customer.discountLoyaltyCard match {
        case Some(card) =>
          val discountPercentage = card.stars * 0.02
          val discountedFoodTotal = foodTotal * (1 - discountPercentage)
          discountedFoodTotal + drinksTotal
        case None => orderTotal
      }
    }
  }




  // apply staff discount to total bill
  def eligibleForDiscount(staff: Staff): Either[String,Boolean]={
    staff match {
      case Staff  (_,age,_,_,_,monthsWorked) if monthsWorked >= 6 && age >=18 => Right(true)
      case Staff (_,age,_,_,_,monthsWorked) if monthsWorked >age => Left("Illegal: Months worked is higher than the age")
      case _ => Right(false)
    }
  }

  def applyStaffDiscount(staff: Staff, totalBill: Double): Either[String,Double] = {
    eligibleForDiscount(staff) match {
      case Right(true) => Right(totalBill*0.9)
      case Right(false) => Right(totalBill)
      case Left(error) => Left(error)
    }
    }

}

