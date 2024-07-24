import scala.util.Random

object Main extends App {

  case class CafeMenu(name: String, price: Double, isHot: Boolean, isFood: Boolean, premium: Boolean)

  val menu: List[CafeMenu] = List(
    CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true),
    CafeMenu("espresso", 2.00, isHot = true, isFood = false, premium = true),
    CafeMenu("sandwich", 9.50, isHot = false, isFood = true, premium = false),
    CafeMenu("iced coffee", 4.50, isHot = false, isFood = false, premium = true),
    CafeMenu("muffin", 1.75, isHot = false, isFood = true, premium = false),
    CafeMenu("tea", 1.25, isHot = true, isFood = false, premium = false),
    CafeMenu("latte", 3.00, isHot = true, isFood = false, premium = true),
    CafeMenu("bagel", 2.00, isHot = false, isFood = true, premium = false),
    CafeMenu("hot chocolate", 2.75, isHot = true, isFood = false, premium = true),
    CafeMenu("smoothie", 3.50, isHot = false, isFood = false, premium = false)
  )


  def addPremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu :+ specialItem
  }

  def removePremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu.filterNot(_ == specialItem)
  }

  def createRandomOrder(menu: List[CafeMenu], numberOfItems: Int): List[CafeMenu] = {
    Random.shuffle(menu).take(numberOfItems)
  }

  def calculateServiceCharge(order: List[CafeMenu]): Double = {
    val total = order.map(_.price).sum
    val hasHotDrinks = order.exists(item => item.isHot && !item.isFood)
    val hasColdFood = order.exists(item => !item.isHot && item.isFood)
    val hasHotFood = order.exists(item => item.isHot && item.isFood)
    val hasPremiumSpecial = order.exists(_.premium)

    val hotFoodCharge = if (hasHotFood) math.min(total * 0.20, 20.0) else 0.0
    val premiumSpecialCharge = if (hasPremiumSpecial) math.min(total * 0.25, 40.0) else 0.0
    val otherCharge = if (hasHotDrinks || hasColdFood) total * 0.10 else 0.0

    math.max(hotFoodCharge, math.max(premiumSpecialCharge, otherCharge))
  }

  def calculateTotal(order: List[CafeMenu]): Double = {
    order.map(_.price).sum
  }

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
