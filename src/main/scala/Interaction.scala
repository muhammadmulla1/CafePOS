import Main._
import scala.io.StdIn.readLine

object Interaction extends App{

  def findMenuItems(name:String):Option[CafeMenu]= {
    menu.find(_.name.equalsIgnoreCase(name))
  }

  println("Welcome to the Cafe POS system!")
  println("Current Menu: ")
  



}