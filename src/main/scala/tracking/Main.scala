package tracking

import cats._
import cats.data._
import cats.implicits._

object Tracked {

  type Tracked[A] = Writer[List[String], A]

  implicit def trackedShow[A: Show]: Show[Tracked[A]] =
    Show.show { ta =>
      val (log, a) = ta.run
      (log ++ List(a.show)).mkString("\n")
    }

}

import Tracked._

case class Client(id: Long, name: String, age: Int)
object Client {

  def makeRaw(id: Long, name: String, age: Int): Tracked[Client] =
    // Client(id, name, age).pure[Tracked]
    // Writer(List("Creating client"), Client(id, name, age))
    // List("Creating client").tell.map(_ => Client(id, name, age))
    Client(id, name, age).writer(List("Creating client"))
}

case class Product(id: Long, name: String, unitPrice: Double)
object Product {

  def makeRaw(id: Long, name: String, unitPrice: Double): Tracked[Product] =
    Product(id, name, unitPrice).writer(List("Creating product"))
}

case class ShoppingCartItem(quantity: Int, product: Product) {

  def total: Double = quantity * product.unitPrice
}
object ShoppingCartItem {

  implicit val ShoppingCartItemShow: Show[ShoppingCartItem] =
    Show.show(item => s"${item.quantity} x ${item.product.name}")

  def makeRaw(
      quantity: Int,
      productId: Long,
      productName: String,
      productUnitPrice: Double
    ): Tracked[ShoppingCartItem] =
    for {
      _       <- List("Creating shopping cart item").tell
      product <- Product.makeRaw(productId, productName, productUnitPrice)
    } yield ShoppingCartItem(quantity, product)

}

case class ShoppingCart(client: Client, items: List[ShoppingCartItem]) {

  def total = items.map(_.total).sum
}
object ShoppingCart {

  def makeRaw(
      clientId: Long,
      clientName: String,
      clientAge: Int,
      items: List[(Int, Long, String, Double)]
    ): Tracked[ShoppingCart] =
    for {
      _       <- List("Creating shopping cart").tell
      client  <- Client.makeRaw(clientId, clientName, clientAge)
      scItems <- items.traverse(x => ShoppingCartItem.makeRaw(x._1, x._2, x._3, x._4))
    } yield ShoppingCart(client, scItems)

  implicit val scShow: Show[ShoppingCart] = Show.fromToString

}

sealed trait Discount {

  val name: String

  def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean

  def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double

  def calculatedDiscount(client: Client, shoppingCartItem: ShoppingCartItem): Tracked[Double] =
    if (applies(client, shoppingCartItem))
      getDiscountedAmount(shoppingCartItem)
        .writer(List(s"Applied discount: $name"))
    else 0d.pure[Tracked]

}
object Discount {

  object MoreThanFiveUnitsDiscount extends Discount {

    override val name: String = "10% discount on 5+ units"

    override def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean =
      shoppingCartItem.quantity >= 5

    override def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double =
      shoppingCartItem.total * 0.1

  }

  object ElderlyDiscount extends Discount {

    override val name: String = "20% discount for 65+ people"

    override def applies(client: Client, shoppingCartItem: ShoppingCartItem): Boolean =
      client.age > 65

    override def getDiscountedAmount(shoppingCartItem: ShoppingCartItem): Double =
      shoppingCartItem.total * 0.2

  }

  val allDiscounts = List(MoreThanFiveUnitsDiscount, ElderlyDiscount)

}

object Main extends App {
  def calculateTotalDiscount(
      shoppingCart: ShoppingCart,
      discounts: List[Discount]
    ): Tracked[Double] =
    (shoppingCart.items, discounts)
      .mapN((item, discount) => discount.calculatedDiscount(shoppingCart.client, item))
      .combineAll

  def calculateTotal(shoppingCart: ShoppingCart): Tracked[Double] =
    calculateTotalDiscount(shoppingCart, Discount.allDiscounts)
      .map(shoppingCart.total - _)

  val client       = Client(1, "albert", 70)
  val milk         = Product(1, "milk", 15.0)
  val eggs         = Product(2, "eggs", 25.0)
  val items        = List(ShoppingCartItem(10, milk), ShoppingCartItem(30, eggs))
  val shoppingCart = ShoppingCart(client, items)

  // println(calculateTotalDiscount(shoppingCart, Discount.allDiscounts).show)
  // println(calculateTotal(shoppingCart).show)
  println(Show[Tracked[Double]].show(calculateTotalDiscount(shoppingCart, Discount.allDiscounts)))
  println(Show[Tracked[Double]].show(calculateTotal(shoppingCart)))

  val newCart =
    ShoppingCart.makeRaw(1, "albert", 70, List((1, 10, "milk", 15.0), (2, 30, "eggs", 25)))
  println(Show[Tracked[ShoppingCart]].show(newCart))
}
