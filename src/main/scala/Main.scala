object Main extends App {

  case class CafeMenu(name: String, price: Double, isHot: Boolean, isFood: Boolean, premium: Boolean)

  val list: List[CafeMenu] = List(
    CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true)
  )

  def addPremiumSpecial(specialItem: CafeMenu, menu: List[CafeMenu]): List[CafeMenu] = {
    menu :+ specialItem.copy(premium = true)
  }

  def removeItem(itemName: String, menu: List[CafeMenu]): List[CafeMenu] = {
    menu.filterNot(_.name.equalsIgnoreCase(itemName))
  }

  val newItem = CafeMenu("premium coffee", 3.00, isHot = true, isFood = false, premium = false)
  val updatedMenu = addPremiumSpecial(newItem, list)
  val menuAfterRemoval = removeItem("cake", updatedMenu)


}
