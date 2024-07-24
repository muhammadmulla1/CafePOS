object Main extends App {

  case class CafeMenu(name: String, price: Double, isHot: Boolean, isFood: Boolean, premium: Boolean)



  val list: List[CafeMenu] = List(
  CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true)
  )


  def addPremiumSpecial(specialItem: String, menu: List[CafeMenu]): List = {

    menu:+ specialItem

  }


}
