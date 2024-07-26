object Models {


  case class DrinksLoyaltyCard(stamps: Int = 0)

  case class DiscountLoyaltyCard(stars: Int = 0)

  case class Customer(name: String, age: Int, purchaseHistory: List[CafeMenu], drinksLoyaltyCard: Option[DrinksLoyaltyCard], discountLoyaltyCard: Option[DiscountLoyaltyCard])

  case class Staff(name: String, age: Int, purchaseHistory: List[CafeMenu], drinksLoyaltyCard: Option[DrinksLoyaltyCard], discountLoyaltyCard: Option[DiscountLoyaltyCard], monthsWorked: Int)

  case class CafeMenu(name: String, price: Double, category: Category.Category, temperature: Temperature.Temperature, premium: Boolean)

  object Category extends Enumeration {
    type Category = Value
    val Food, Drink = Value
  }

  object Temperature extends Enumeration {
    type Temperature = Value
    val Hot, Cold = Value
  }

  object LoyaltyCardType extends Enumeration {
    type LoyaltyCardType = Value
    val Drinks, Discount = Value
  }

  object Currency extends Enumeration {
    type Currency = Value
    val GBP, EUR, USD, YEN = Value
  }
}
