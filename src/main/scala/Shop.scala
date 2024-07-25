//import scala.io.StdIn.readLine
//
//object InteractiveConsole extends App {
//  import Main._
//  import Models._
//
//  println("Employee Discount Eligibility Checker System")
//
//  val name = readLine(" enter your full name: ").toLowerCase()
//  val age = readLine("enter your age: ").toInt
//  val monthsWorked = readLine("How many months have you worked?: ").toInt
//
//  val staff = Staff(name, age, List(), None, None, monthsWorked)
//
//  if (eligibleForDiscount(staff)) {
//    println(s"Hello, $name! you are eligible for the staff discount")
//  } else {
//    println(s"$name -  you are not eligible for the staff discount. :(")
//  }
//}
