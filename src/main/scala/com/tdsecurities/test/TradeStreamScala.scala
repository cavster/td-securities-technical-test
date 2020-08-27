package com.tdsecurities.test

/**
  * The system is event based so trades arrive at this consumer out of order.
  * To fix this issue, tradesStream must be sorted with the following rules:
  *
  * * SWAP trades are represented by two strings (i.e. 123CB SELL or 9898AA BUY), orderNumber and buySell respectively.
  *   - order numbers must be sorted lexicographically (i.e 11 before 11C)
  *   - Sells (SELL) must be processed before Buys (BUY) by stacking SELL/BUY pairs with the same orderNumber
  *
  * * CASH trades are represented by two numbers (i.e. 984756 34566), code and serial respectively.
  *   CASH trades must be processed in order of appearance, after all SWAPs are processed
  *
  * Please take a look at the test class for some examples
  */


object TradeStreamScala  {

  sealed trait Order {
    val raw: String
  }
  sealed trait BuySellOrder extends Order{
    val orderNumber: String
  }
  final case class Cash(code: String, serial: String, override val raw: String) extends Order
  final case class Sell(override val orderNumber: String, override val raw: String) extends BuySellOrder
  final case class Buy(override val orderNumber: String, override val raw: String) extends BuySellOrder

  val ordering: Ordering[Order] = Ordering.by {
    case o: Sell => (1, o.orderNumber, 1)
    case o: Buy => (1,  o.orderNumber, 2)
    case _: Cash => (2,  "", 1)
  }

    def orderTrades(tradesStream: String*): Seq[String] = {
      tradesStream
        .map(parse)
        .sorted(ordering)
        .map(_.raw)
  }

  def parse(tradesString: String):Order = {
    val split =     tradesString.split(" ")
    (split(0),split(1)) match  {
      case (on,"BUY") =>Buy(on,tradesString)
      case (on,"SELL") =>Sell(on,tradesString)
      case (code,serial) =>Cash(code,serial,tradesString)
    }
  }

}
