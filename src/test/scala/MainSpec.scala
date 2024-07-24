import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import Main._

class MainSpec extends AnyWordSpec with Matchers {

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
      val totalBill = Main.calculateTotal(order)
      assert(totalBill == Right(3.50))
    }

    "return an error message if the order is empty" in {
      val emptyOrder = List.empty[CafeMenu]
      val totalBill = calculateTotal(emptyOrder)
      assert(totalBill == Left("The order is empty."))
    }
  }

  "calculateServiceCharge" should {
    "apply no service charge if all items are cold drinks" in {
      val order = List(
        CafeMenu("iced coffee", 2.50, isHot = false, isFood = false, premium = false),
        CafeMenu("smoothie", 3.50, isHot = false, isFood = false, premium = false)
      )
      calculateServiceCharge(order) shouldEqual 0.0
    }

    "apply a 10% service charge for hot drinks or cold food" in {
      val order = List(
        CafeMenu("tea", 1.25, isHot = true, isFood = false, premium = false),
        CafeMenu("muffin", 1.75, isHot = false, isFood = true, premium = false)
      )
      calculateServiceCharge(order) shouldEqual 0.10 * 3.00
    }

    "apply a 20% service charge for hot food with a maximum of £20" in {
      val order = List(
        CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = false),
        CafeMenu("sandwich", 3.50, isHot = false, isFood = true, premium = false)
      )
      calculateServiceCharge(order) shouldEqual math.min(0.20 * 5.00, 20.0)
    }

    "apply a 25% service charge for premium specials with a maximum of £40" in {
      val order = List(
        CafeMenu("premium tea", 2.50, isHot = true, isFood = false, premium = true)
      )
      calculateServiceCharge(order) shouldEqual math.min(0.25 * 2.50, 40.0)
    }

    "apply the correct service charge if items purchased are mixed" in {
      val order = List(
        CafeMenu("premium coffee", 1.50, isHot = true, isFood = false, premium = true),
        CafeMenu("muffin", 1.75, isHot = false, isFood = true, premium = true)
      )
      calculateServiceCharge(order) shouldEqual math.min(0.25 * 4.75, 40.0)
    }
  }

  "calculateFinalCharge" should {
    "return the total amount including a service charge" in {
      val order = List(
        CafeMenu("cake", 1.50, isHot = true, isFood = true, premium = false),
        CafeMenu("espresso", 2.00, isHot = true, isFood = false, premium = false)
      )
      val total = order.map(_.price).sum
      val serviceCharge = calculateServiceCharge(order)
      calculateFinalCharge(order) shouldEqual total + serviceCharge
    }
  }
}