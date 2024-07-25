import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import Models._
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

  "checkLoyaltyCardEligibility" should {
    "return true for a customer eligible for a drinks loyalty card" in {
      val customer = Customer("Arei Mohammed", 25, List.fill(5)(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), None, None)
      checkLoyaltyCardEligibility(customer, "drinks") shouldBe true
    }

    "return false for a customer not eligible for a drinks loyalty card due to age" in {
      val customer = Customer("Bilal Amiry", 16, List.fill(5)(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), None, None)
      checkLoyaltyCardEligibility(customer, "drinks") shouldBe false
    }

    "return false for a customer not eligible for a drinks loyalty card due to insufficient purchases" in {
      val customer = Customer("Arei Mohammed", 25, List.fill(2)(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), None, None)
      checkLoyaltyCardEligibility(customer, "drinks") shouldBe false
    }

    "return false for a customer not eligible for a discount loyalty card due to having a discount loyalty card" in {
      val customer = Customer("Jane Doe", 25, List.fill(5)(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), None, Some(DiscountLoyaltyCard()))
      checkLoyaltyCardEligibility(customer, "discount") shouldBe false
    }

    "return true for a customer eligible for a discount loyalty card" in {
      val customer = Customer("John Smith", 30, List.fill(5)(CafeMenu("sandwich", 30.0, Category.Food, Temperature.Cold, premium = false)), None, None)
      checkLoyaltyCardEligibility(customer, "discount") shouldBe true
    }

    "return false for a customer not eligible for a discount loyalty card due to insufficient total spend" in {
      val customer = Customer("John Smith", 30, List.fill(5)(CafeMenu("sandwich", 10.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard()))
      checkLoyaltyCardEligibility(customer, "discount") shouldBe false
    }

    "return false for a customer not eligible for a discount loyalty card due to having a drinks loyalty card" in {
      val customer = Customer("John Smith", 30, List.fill(3)(CafeMenu("sandwich", 30.0, Category.Food, Temperature.Cold, premium = false)), Some(DrinksLoyaltyCard()), None)
      checkLoyaltyCardEligibility(customer, "discount") shouldBe false
    }
  }

  "updateDrinksLoyaltyCard" should {
    "add a stamp to a customer's drinks loyalty card" in {
      val customer = Customer("Arei Mohammed", 30, List(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), Some(DrinksLoyaltyCard(2)), None)
      val updatedCustomer = updateDrinksLoyaltyCard(customer)
      updatedCustomer.drinksLoyaltyCard.get.stamps shouldBe 3
    }

    "resets the stamps to 0 after the 10th stamp" in {
      val customer = Customer("Arei Mohammed", 30, List(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), Some(DrinksLoyaltyCard(10)), None)
      val updatedCustomer = updateDrinksLoyaltyCard(customer)
      updatedCustomer.drinksLoyaltyCard.get.stamps shouldBe 0
    }

    "do nothing if the customer does not have a drinks loyalty card" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("tea", 1.25, Category.Drink, Temperature.Hot, premium = false)), None, None)
      val updatedCustomer = updateDrinksLoyaltyCard(customer)
      updatedCustomer.drinksLoyaltyCard shouldBe None
    }
  }

  "applyStar" should {
    "add a star to the customer's discount loyalty card if the bill is more than £20" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 30.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard(7)))
      val updatedCustomer = updateDiscountLoyaltyCard(customer, 21.0)
      updatedCustomer.discountLoyaltyCard.get.stars shouldBe 8
    }

    "not add a star to a customer's discount loyalty card if the bill is £20 or less" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 20.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard(7)))
      val updatedCustomer = updateDiscountLoyaltyCard(customer, 20.0)
      updatedCustomer.discountLoyaltyCard.get.stars shouldBe 7
    }

    "not add more stars if the customer already has 8 stars" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 20.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard(8)))
      val updatedCustomer = updateDiscountLoyaltyCard(customer, 20.0)
      updatedCustomer.discountLoyaltyCard.get.stars shouldBe 8
    }

    "do nothing if the customer does not have discount loyalty card" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 20.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard(7)))
      val updatedCustomer = updateDiscountLoyaltyCard(customer, 20.0)
      updatedCustomer.discountLoyaltyCard.get.stars shouldBe 7
    }
  }

  "calculateDiscount" should {
    "apply the correct discount based on the number of stars" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 30.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard(4)))
      val orderTotal = 100.0
      val expectedDiscount = 0.08 * orderTotal
      applyDiscount(customer, orderTotal) shouldBe orderTotal - expectedDiscount
    }

    "apply the maximum discount of 16% if the customer has 8 stars" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 30.0, Category.Food, Temperature.Cold, premium = false)), None, Some(DiscountLoyaltyCard(8)))
      val orderTotal = 100.0
      val expectedDiscount = 0.16 * orderTotal
      applyDiscount(customer, orderTotal) shouldBe orderTotal - expectedDiscount
    }

    "apply no discount if the customer does not have a discount loyalty card" in {
      val customer = Customer("John Doe", 30, List(CafeMenu("sandwich", 30.0, Category.Food, Temperature.Cold, premium = false)), None, None)
      val orderTotal = 100.0
      applyDiscount(customer, orderTotal) shouldBe orderTotal
    }
  }

  "applyStaffDiscount" should {
    "apply an additional 10% discount if the staff member has worked for 6 months or more" in {
      val totalBill = 33.0
      val staff = Staff("Arei Smith", 25, List(), None, None, monthsWorked = 8)
      val discountedBill = applyStaffDiscount(staff, totalBill)
      discountedBill shouldBe Right(29.7)
    }

    "not apply the discount if the staff member has worked for less than 6 months" in {
      val totalBill = 33.0
      val staff = Staff("John Doe", 30, List(), None, None, monthsWorked = 5)
      val discountedBill = applyStaffDiscount(staff, totalBill)
      discountedBill shouldBe Right(33.0) // No discount applied
    }

    "not apply the discount if the person is not a staff member" in {
      val totalBill = 33.0
      val staff = Staff("Jane Doe", 30, List(), None, None, monthsWorked = 0)
      val discountedBill = applyStaffDiscount(staff, totalBill)
      discountedBill shouldBe Right(33.0) // No discount applied
    }
  }

  "applyCurrencyConversion" should {
    "convert the total bill to the specified currency" in {
      val totalBillGBP = 33.0

      val convertedToEUR = applyCurrencyConversion(totalBillGBP, Currency.EUR)
      val expectedEUR = totalBillGBP * currencyExchange(Currency.EUR)
      convertedToEUR shouldBe expectedEUR

      val convertedToUSD = applyCurrencyConversion(totalBillGBP, Currency.USD)
      val expectedUSD = totalBillGBP * currencyExchange(Currency.USD)
      convertedToUSD shouldBe expectedUSD

      val convertedToYEN = applyCurrencyConversion(totalBillGBP, Currency.YEN)
      val expectedYEN = totalBillGBP * currencyExchange(Currency.YEN)
      convertedToYEN shouldBe expectedYEN
    }
  }
}
