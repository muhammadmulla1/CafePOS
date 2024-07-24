import org.scalatest.wordspec.AnyWordSpec
import Main._


class MainSpec extends AnyWordSpec{



  "cafe menu " should{
    "retrieve menu item" in {
      val menu = Main.list
      assert(menu.nonEmpty)
      assert(menu.size == 10)
      assert(menu.head.name == "cake")
    }

    "remove item from the menu" in {// Arwi will come back to this later
      val item1 = CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true)
      val item2 = CafeMenu("tea", 1.25, isHot = true, isFood = false, premium = false)
      val menu = List(item1, item2)
      val updatedMenu = menu.filterNot(_ == item2)
      assert(updatedMenu.size == 1)
      assert(updatedMenu.contains(item1))
      assert(!updatedMenu.contains(item2))
    }
    "adds special item in the menu" in{
      val newItem = CafeMenu("steak", 20.99, isHot = true, isFood = true, premium = true)
      val updatedMenu = Main.addPremiumSpecial(newItem, Main.updatedMenu)
      assert(updatedMenu.contains(newItem))


    }
    "removes special item in the menu" in{

    }
  }

  "cafe bill " should{
    "produce an itemised bill without service charge " in {

    }
    "produce an itemised bill with service charge " in {

    }
  }
}
