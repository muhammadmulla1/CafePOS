import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Main._

class MainSpec extends AnyWordSpec with Matchers {

  "CafeMenu" should {
    "have a list of items" in {
      val menuItems = Main.menu
      menuItems should not be empty
    }
  }

  "addPremiumSpecial" should {
    "add a premium item to the menu" in {
      val specialItem = CafeMenu("premium tea", 2.50, Category.Drink, Temperature.Hot, premium = true)
      val updatedMenu = addPremiumSpecial(specialItem, menu)
      updatedMenu should contain(specialItem)
      updatedMenu.size shouldBe menu.size + 1
    }
  }

  "removePremiumSpecial" should {
    "remove a premium item from the menu" in {
      val specialItem = CafeMenu("premium coffee", 3.00, Category.Drink, Temperature.Hot, premium = true)
      val updatedMenu = removePremiumSpecial(specialItem, menu :+ specialItem)
      updatedMenu should not contain specialItem
      updatedMenu.size shouldBe menu.size
    }
  }

  "createRandomOrder" should {
    "create a random order with the specified number of items" in {
      val orderSize = 3
      val randomOrder = createRandomOrder(menu, orderSize)
      randomOrder.size shouldBe orderSize
    }

    "not exceed the size of the menu" in {
      val orderSize = menu.size + 5
      val randomOrder = createRandomOrder(menu, orderSize)
      randomOrder.size shouldBe menu.size
    }
  }

  "generateBill" should {
    "calculate the total bill for the order" in {
      val order = List(
        CafeMenu("cake", 1.50, Category.Food, Temperature.Cold, premium = false),
        CafeMenu("espresso", 2.00, Category.Drink, Temperature.Hot, premium = false)
      )
      val totalBill = calculateTotal(order)
      totalBill shouldBe 3.50
    }

    "return an error message if the order is empty" in {
      val emptyOrder = List.empty[CafeMenu]
      val totalBill = calculateTotal(emptyOrder)
      totalBill shouldBe 0.0
    }


  }

  "calculateServiceCharge" should {
    "apply no service charge if all items are cold drinks" in {
      val order = List(
        CafeMenu("iced coffee", 2.50, Category.Drink, Temperature.Cold, premium = false),
        CafeMenu("smoothie", 3.50, Category.Drink, Temperature.Cold, premium = false)
      )
      calculateServiceCharge(order) shouldEqual 0.0
    }

    "apply a 10% service charge for hot drinks or cold food" in {
      val order = List(
        CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false),
        CafeMenu("muffin", 1.75, Category.Food, Temperature.Cold, premium = false)
      )
      calculateServiceCharge(order) shouldEqual 0.10 * 3.00
    }

    "apply a 20% service charge for hot food with a maximum of £20" in {
      val order = List(
        CafeMenu("cake", 1.50, Category.Food, Temperature.Hot, premium = false),
        CafeMenu("sandwich", 3.50, Category.Food, Temperature.Cold, premium = false)
      )
      calculateServiceCharge(order) shouldEqual math.min(0.20 * 5.00, 20.0)
    }

    "apply a 25% service charge for premium specials with a maximum of £40" in {
      val order = List(
        CafeMenu("premium tea", 2.50, Category.Drink, Temperature.Hot, premium = true)
      )
      calculateServiceCharge(order) shouldEqual math.min(0.25 * 2.50, 40.0)
    }

    "apply the correct service charge if items purchased are mixed" in {
      val order = List(
        CafeMenu("premium coffee", 1.50, Category.Drink, Temperature.Hot, premium = true),
        CafeMenu("muffin", 1.75, Category.Food, Temperature.Cold, premium = false)
      )
      val total = order.map(_.price).sum
      val expectedServiceCharge = math.min(total * 0.25, 40.0)
      calculateServiceCharge(order) shouldEqual expectedServiceCharge
    }

  }

  "calculateFinalCharge" should {
    "return the total amount including a service charge" in {
      val order = List(
        CafeMenu("cake", 1.50, Category.Food, Temperature.Cold, premium = false),
        CafeMenu("espresso", 2.00, Category.Drink, Temperature.Hot, premium = false)
      )
      val total = calculateTotal(order)
      val serviceCharge = calculateServiceCharge(order)
      calculateFinalCharge(order) shouldBe total + serviceCharge
    }
  }
}
