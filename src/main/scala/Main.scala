import scala.util.Random

object Main  {

  object Category extends Enumeration {
    type Category = Value
    val Food, Drink = Value
  }

  object Temperature extends Enumeration {
    type Temperature = Value
    val Hot, Cold = Value
  }

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
}

