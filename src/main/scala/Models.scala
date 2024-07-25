object Models {

  case class Customer(name: String,
                      age: Int,
                      purchaseHistory: List[CafeMenu],
                      drinksLoyaltyCard: Option[DrinksLoyaltyCard],
                      discountLoyaltyCard: Option[DiscountLoyaltyCard])

  case class Staff(name: String,
                   age: Int,
                   purchaseHistory: List[CafeMenu],
                   drinksLoyaltyCard: Option[DrinksLoyaltyCard],
                   discountLoyaltyCard: Option[DiscountLoyaltyCard],
                   monthsWorked: Int)

  case class DrinksLoyaltyCard(stamps: Int = 0)
  case class DiscountLoyaltyCard(stars: Int = 0)
  case class CafeMenu(
                       name: String,
                       price: Double,
                       category: Category.Category,
                       temperature: Temperature.Temperature,
                       premium: Boolean)

}