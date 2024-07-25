import sun.util.calendar.BaseCalendar.Date

import scala.util.Random

object Main {

  import Category._
  import Temperature._
  import LoyaltyCardType._

  case class Customer(name: String,
                      age: Int,
                      purchaseHistory: List[CafeMenu],
                      drinksLoyaltyCard: Option[DrinksLoyaltyCard],
                      discountLoyaltyCard: Option[DiscountLoyaltyCard]
                     )

  case class DrinksLoyaltyCard(stamps: Int = 0)

  case class DiscountLoyaltyCard(stars: Int = 0)

  case class CafeMenu(
                       name: String,
                       price: Double,
                       category: Category.Category,
                       temperature: Temperature.Temperature,
                       premium: Boolean
                     )

  // Sample menu items
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

  // Add premium special
  def addPremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu :+ specialItem
  }

  // Remove premium special
  def removePremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu.filterNot(_ == specialItem)
  }

  // Create a random order
  def createRandomOrder(menu: List[CafeMenu], numberOfItems: Int): List[CafeMenu] = {
    Random.shuffle(menu).take(numberOfItems)
  }

  // Calculate service charge
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

  // Calculate final charge
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
      case "drinks" => customer.age >= 18 &&
        customer.purchaseHistory.count(_.category == Category.Drink) >= 5 &&
        customer.drinksLoyaltyCard.isEmpty
      case "discount" => customer.age >= 18 &&
        customer.purchaseHistory.size >= 5 &&
        customer.purchaseHistory.size >= 150 &&
        customer.discountLoyaltyCard.isEmpty
      case _ => false
    }
  }

  def updateDrinksLoyaltyCard(customer: Customer): Customer = {
    customer.drinksLoyaltyCard match {
      case Some(card) if card.stamps < 9 =>
        val newStamps = card.stamps +1
        val updatedCard = card.copy(stamps = card.stamps)
        customer.copy(drinksLoyaltyCard = Some(updatedCard))
      case Some(card) if card.stamps == 9 =>
        val updatedCard = card.copy(stamps = 0)
        customer.copy(drinksLoyaltyCard = Some(updatedCard))
      case Some(card) => customer
      case None => customer
    }
  }


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

  def applyDiscount(customer: Customer, orderTotal: Double): Double = {
    customer.discountLoyaltyCard match {
      case Some(card) =>
        val discountPercentage = card.stars * 0.02
        val discount = orderTotal * discountPercentage
        orderTotal - discount
      case None => orderTotal // No discount card
    }
  }
}

