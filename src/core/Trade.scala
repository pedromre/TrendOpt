package core

 

class Trade(var tradeType:String, var candle:Candle, var amountB:Float, var amountS:Float, var price:Float, var market:String=null, var link:Trade = null) {
 
  def setLink(trade:Trade){
    link = trade
  }
}