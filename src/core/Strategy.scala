package core

import scala.collection.mutable.ListBuffer

import java.io._

class Strategy(var cs:CandleSeries, var name:String = null, var initialCapital:Float = 0) {
  var buyList:ListBuffer[Buy] = new ListBuffer[Buy]
  var activeTrades = ListBuffer.empty[Buy] 
  var activeTradesTest = ListBuffer.empty[Trade]
  var sellList:ListBuffer[Buy] = new ListBuffer[Buy]
  var tradeList:ListBuffer[Trade] = new ListBuffer[Trade]
  var description:String = null
  var exitRule:Rule = null
  var exitRuleList = ListBuffer.empty[Rule] // Testes
  var entryRuleList = ListBuffer.empty[Rule] // Testes
  var entryRule:Rule = null
  var candles = cs.CS.toArray
  var funds = initialCapital
  var stocks = 0f
  var historic = new ListBuffer[(Float,Float, String, String)]
  var historicTest = new ListBuffer[(Trade, Float, Float)] //Stores candle, buying price, funds, stocks, type
  var stopLoss:Float = 100f
  var stopProfit:Float = Float.MaxValue
  
  var firstCandle:Candle = null
  var lastCandle:Candle = null
  
  def checkStopLoss(buy:Buy, index:Int):Boolean = {
    var lowestPrice = candles(index).low
    var buyingPrice = buy.price
    if(lowestPrice < buyingPrice){
      var lossPercent = 100f - (lowestPrice/buyingPrice)*100f
      if(stopLoss < lossPercent) return true
    }
    return false
  }
  
  def checkStopProfit(buy:Buy, index:Int):Boolean = {
    var highestPrice = candles(index).high
    var buyingPrice = buy.price
    if(highestPrice > buyingPrice){
      var winPercent = (highestPrice/buyingPrice)*100f - 100f
      if(winPercent > stopProfit) return true
    }
    return false
  }
  
  def setExitRule(parameter1:ListBuffer[Float], action:String, parameter2:ListBuffer[Float]) = {
    exitRule = new RuleCross("Exit")
    exitRule.asInstanceOf[RuleCross].setRule(parameter1, action, parameter2)
  }
  
  def setEntryRule(parameter1:ListBuffer[Float], action:String, parameter2:ListBuffer[Float]) = {
    entryRule = new RuleCross("Entry")
    entryRule.asInstanceOf[RuleCross].setRule(parameter1, action, parameter2)
  }
  
  def setEntryRuleTestUnder(parameter1:ListBuffer[Float], threshold:Float) = {
    entryRule = new RuleUnderThreshold()
    entryRule.asInstanceOf[RuleUnderThreshold].setRule(parameter1, threshold)
    entryRuleList.append(entryRule)
  }
  
  def setExitRuleTestOver(parameter1:ListBuffer[Float], threshold:Float) = {
    exitRule = new RuleOverThreshold()
    exitRule.asInstanceOf[RuleOverThreshold].setRule(parameter1, threshold)
    exitRuleList.append(exitRule)
  }
  
  def setStopLoss(value:Float){
    stopLoss = value
  }
  
  def setStopProfit(value:Float){
    stopProfit = value
  }
  
  def checkFunds(amountB:Float):Boolean = {
    if(amountB<=funds) return true
    return false
  }
  
  def checkStocks(amount:Float):Boolean = {
    if(amount<=stocks) return true
    return false
  }
  
  def sell(price:Float, trade:Buy, i:Int){
    var Sell = trade.quantityS
    var amountB = trade.quantityS * price 
    var amountS = trade.quantityS
    if(checkStocks(Sell)){
           sellList.append(new Buy(candles(i).close, amountS, amountB, candles(i))) 
           funds = funds + amountB
           stocks = stocks - amountS
           historic.append((funds, stocks, candles(i).timeStamp, "Sell"))
    }
  }
  
  def simulateStrategy(start:Int, finish:Int) ={
    firstCandle = candles(start)
    lastCandle = candles(finish)
    //Var reseting
    stocks = 0f
    funds = initialCapital
    tradeList.clear()
    activeTradesTest.clear()
    historicTest.clear()
    
    // --------------------------------- TESTE --------------------------------
    for((candle,i) <- candles.zipWithIndex){
      if(i >= start){
        for(rule <- entryRuleList){
        if(rule.shouldAct(i)){
          var Buy = funds
          var amountB = funds
          var amountS = Buy / candle.close //Change for different entry prices
          var price = candle.close
          if(checkFunds(Buy) && Buy != 0){
            var t = new Trade("Buy", candle, amountB, amountS, price)
            tradeList.append(t)
            activeTradesTest.append(t)
            funds = funds - Buy
            stocks = stocks + amountS
            historicTest.append((t, funds, stocks))
          } 
        }
      }
      for(rule <- exitRuleList){
        if(rule.shouldAct(i)){
          var Sell = stocks
          var amountB = stocks * candles(i).close
          var amountS = stocks
          var price = candle.close
          if(checkStocks(Sell) && Sell >= 0.001f){
             var t = new Trade("Sell", candle, amountB, amountS, price)
             tradeList.append(t)
             //Add link between trades ------------------------------------------------------------------
             //t.setLink(activeTradesTest.last)
             //activeTradesTest.last.setLink(t)
             //Remove from active trades ----------------------------------------------------------------
             activeTradesTest.remove(0)
             funds = funds + amountB
             stocks = stocks - amountB/candle.close
             historicTest.append((t, funds, stocks))
          }
        }
      }
      }
      
    }
    
    
    // ------------------------------ FIM DE TESTE ----------------------------
    
//    for(i <- start  to candles.length-1){
//      //Check stop profit and loss
//      //Outdated--------------------------------------------------------
////      for(trade <- activeTrades){
////        if(checkStopLoss(trade, i)){
////          println("Stop Loss activated")
////          //Terminate the trade
////          var stopLossPrice = ((100 - stopLoss)/100) * trade.price 
////          sell(stopLossPrice, trade, i)
////          //Remove trade from activeTrades
////          activeTrades.remove(activeTrades.indexOf(trade))
////        }else if(checkStopProfit(trade, i)){
////          println("Stop Profit activated")
////          //Terminate the trade
////          var stopProfitPrice = ((100 + stopProfit)/100) * trade.price 
////          sell(stopProfitPrice, trade, i)
////          //Remove trade from activeTrades
////          activeTrades.remove(activeTrades.indexOf(trade))
////        }
////      }
//      for(rule <- entryRuleList){
//        if(rule.shouldAct(i)){
//          var Buy = funds
//          var amountB = funds
//          var amountS = Buy / candles(i).close //Change for different entry prices
//          var price = candles(i).close
//          if(checkFunds(Buy) && Buy != 0){
//            var t = new Trade("Buy", candles(i), amountB, amountS, price)
//            tradeList.append(t)
//            activeTradesTest.append(t)
//            funds = funds - Buy
//            stocks = stocks + amountS
//            historicTest.append((t, funds, stocks))
//          } 
//        }
//      }
//      for(rule <- exitRuleList){
//        if(rule.shouldAct(i)){
//          var Sell = stocks
//          var amountB = stocks * candles(i).close
//          var amountS = stocks
//          var price = candles(i).close
//          if(checkStocks(Sell) && Sell >= 0.001f){
//             var t = new Trade("Sell", candles(i), amountB, amountS, price)
//             tradeList.append(t)
//             //Add link between trades ------------------------------------------------------------------
//             //t.setLink(activeTradesTest.last)
//             //activeTradesTest.last.setLink(t)
//             //Remove from active trades ----------------------------------------------------------------
//             activeTradesTest.remove(0)
//             funds = funds + amountB
//             stocks = stocks - amountB/candles(i).close
//             historicTest.append((t, funds, stocks))
//          }
//        }
//      }
//      
//    }
  }
  

  def getHistoric(start:Int){
    if(historicTest.isEmpty) println("Historic is empty - No trades")
    println("First:" + firstCandle.timeStamp + "\tLast:" + lastCandle.timeStamp)
    for((trade,funds,stocks) <- historicTest){
      var t =  trade
      println(funds + " " + stocks + " " + trade.candle.timeStamp + " " + trade.tradeType + " " + trade + " with link with " + trade.link)
    }
  }
  
  def exportToCVS(indicator1:ListBuffer[Float], indicator2:ListBuffer[Float]){
    val writer = new PrintWriter(new File("C:\\Users\\John\\Documents\\Tese\\output.csv"))
      for(i <- 0 to candles.length-1){
        writer.write(candles(i).timeStamp + "," + candles(i).open + "," + candles(i).high + "," + candles(i).low + "," + candles(i).close + "," + indicator1(i) + "," + indicator2(i) + "\n" )
      }
      
      writer.close()
  }
  
  def resetStrategy(){
    exitRuleList.clear() // Testes
    entryRuleList.clear() // Testes
  }
  
  def getROI(): Float = {
    if(historicTest.isEmpty) return initialCapital
    var total = funds
    if(stocks > 0.00001f) total = lastCandle.close * stocks + total
    return funds + (stocks * candles.last.close)
  }
  
  def getBandH(): Float = {
    if(firstCandle == null | lastCandle == null) return 0
    var stocksHold = initialCapital/firstCandle.close
    return stocksHold * lastCandle.close
  }
  
}