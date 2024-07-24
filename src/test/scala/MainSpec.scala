import org.scalatest.wordspec.AnyWordSpec
import Main._

class MainSpec extends AnyWordSpec {

  "CafeMenu" should {
    "have a list of items" in {
      val menu = Main.list
      assert(menu.nonEmpty)
    }
  }

  "addPremiumSpecial" should {
    "add a premium item to the menu" in {
      val specialItem = CafeMenu("premium tea", 2.50, isHot = true, isFood = false, premium = true)
      val updatedMenu = addPremiumSpecial(specialItem, list)
      assert(updatedMenu.contains(specialItem))
      assert(updatedMenu.size == list.size + 1)
    }
  }

  "removePremiumSpecial" should {
    "remove a premium item from the menu" in {
      val specialItem = CafeMenu("premium coffee", 3.00, isHot = true, isFood = false, premium = true)
      val updatedMenu = removePremiumSpecial(specialItem, list :+ specialItem)
      assert(!updatedMenu.contains(specialItem))
      assert(updatedMenu.size == list.size)
    }
  }

  "createRandomOrder" should {
    "create a random order with the specified number of items" in {
      val orderSize = 3
      val randomOrder = createRandomOrder(list, orderSize)
      assert(randomOrder.size == orderSize)
    }

    "not exceed the size of the menu" in {
      val orderSize = list.size + 5
      val randomOrder = createRandomOrder(list, orderSize)
      assert(randomOrder.size == list.size)
    }
  }

  "generateBill" should {
    "calculate the total bill for the order" in {
      val order = List(
        CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = true),
        CafeMenu("espresso", 2.00, isHot = true, isFood = false, premium = true)
      )
      val totalBill = generateBill(order)
      assert(totalBill == Right(3.50))
    }

    "return an error message if the order is empty" in {
      val emptyOrder = List.empty[CafeMenu]
      val totalBill = generateBill(emptyOrder)
      assert(totalBill == Left("The order is empty."))
    }
  }

}
