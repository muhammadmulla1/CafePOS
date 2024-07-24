object Main extends App {

  case class CafeMenu(name: String, price: Double, isHot: Boolean, isFood: Boolean, premium: Boolean)

  val list: List[CafeMenu] = List(
    CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true)
  )

  def addPremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu :+ specialItem
  }

  val newItem = CafeMenu("premium coffee", 3.00, isHot = true, isFood = false, premium = true)
  val updatedMenu = addPremiumSpecial(newItem, list)
}
