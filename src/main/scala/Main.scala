object Main {

  case class CafeMenu(name: String, price: Double, isHot: Boolean, isFood: Boolean, premium: Boolean)

  val list: List[CafeMenu] = List(
    CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true),
    CafeMenu("espresso", 2.00, isHot = true, isFood = false, premium = true),
    CafeMenu("sandwich", 3.50, isHot = false, isFood = true, premium = false),
    CafeMenu("iced coffee", 2.50, isHot = false, isFood = false, premium = true),
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
//
//
//  def calculateBill(order:List[String], menu: List[CafeMenu]):String = {
//    val items = order.map(itemName => menu.find(_.name = itemName)
//  }
//
//  def createRandomOrder(menu: List[CafeMenu], numberOfItems: Int): List[CafeMenu] = {
//    Random.shuffle(menu).take(numberOfItems)
//  }


}
