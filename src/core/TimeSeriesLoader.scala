package core

import org.ta4j.core._
import java.util.ArrayList;
import java.util.List;
import scala.collection.mutable.ListBuffer
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.ZoneId
import java.time.ZonedDateTime
import org.ta4j.core.BaseTimeSeries
import org.ta4j.core.indicators._
import org.ta4j.core.indicators.helpers._
import scala.collection.mutable.ListBuffer
import java.io._

class TimeSeriesLoader {
  
  var bars: List[Bar] = new ArrayList()
  
  var indicators:List[Indicator[_]] = new ArrayList()
  
  var bts:BaseTimeSeries = null
  //helpers
  var closePrice:ClosePriceIndicator = null
  var typicalPrice:TypicalPriceIndicator = null
  var priceVariation:PriceVariationIndicator = null
  var helperInit = false
  
  def load(){
    var result = new Array[String](1)
    result(0) = "C:\\Users\\John\\Documents\\Tese\\AAPL_data.csv"
    
    var load = ta4jexamples.loaders.CsvBarsLoader.main(result)
  }
  
  def createBTS(cs:CandleSeries, name:String){
    
    
    var candles = cs.CS
    
    for(candle <- candles){
      var open = candle.open.toDouble
      var high = candle.high.toDouble
      var low = candle.low.toDouble
      var close = candle.close.toDouble
      var volume = candle.volume.toDouble
      var date = LocalDate.parse(candle.timeStamp, DateTimeFormatter.ofPattern("yyyy-MM-dd")).atStartOfDay(ZoneId.systemDefault())
      bars.add(new BaseBar(date, open, high, low, close, volume))
    }
    
    bts = new BaseTimeSeries(name, bars)
    
    println("Series: " + bts.getName() + " (" + bts.getSeriesPeriodDescription() + ")")
    println("Number of bars: " + bts.getBarCount())
    println("First bar: \n" + "\tVolume: " + bts.getBar(0).getVolume() + "\n" + "\tOpen price: " + bts.getBar(0).getOpenPrice()+ "\n"
                + "\tClose price: " + bts.getBar(0).getClosePrice())
                
    
    
    
    
//    for(i <- 1 to 20){
//      println(shortSma.getValue(i))
//    }
//    println(shortSma.getTimeSeries().getBarCount)
    
  }
  
  def convertTStoLB(ts:Indicator[_]): ListBuffer[Float]={
    var list = ListBuffer.empty[Float]
    for(e <- 0 to ts.getTimeSeries.getBarCount - 1){
      list.append(ts.getValue(e).toString().toFloat)
    }
    return list
  }
  
  def createSMA(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create SMA
    var sma = new SMAIndicator(closePrice, num)
    indicators.add(sma)
    return sma
  }
  
  def createEMA(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create EMA
    var ema = new EMAIndicator(closePrice, num)
    indicators.add(ema)
    return ema
  }

  def createPPO(fast:Int=12, slow:Int=26): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create PPO
    var ppo = new PPOIndicator(closePrice, fast, slow)
    indicators.add(ppo)
    return ppo
  }

  def createROC(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create ROC
    var  roc = new ROCIndicator(closePrice, num)
    indicators.add(roc)
    return roc
  }
  
  def createRSI(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create RSI
    var  rsi = new RSIIndicator(closePrice, num)
    indicators.add(rsi)
    return rsi
  }
  
  def createWilliamsR(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create WilliamsR
    var  williamsR = new WilliamsRIndicator(bts, num)
    indicators.add(williamsR)
    return williamsR
  }
  
  def createATR(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create ATR
    var  atr = new ATRIndicator(bts, num)
    indicators.add(atr)
    return atr
  }
  
  def createStandardDeviation(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create SD
    var  sd = new statistics.StandardDeviationIndicator(closePrice, num)
    indicators.add(sd)
    return sd
  }
  
  def createCCI(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create CCI
    var  cci = new CCIIndicator(bts, num)
    indicators.add(cci)
    return cci
  }
  
  def createStochasticOscillatorK(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create StochasticOscillatorKIndicator
    var sok = new StochasticOscillatorKIndicator(bts, num)
    indicators.add(sok)
    return sok
  }
  
  def createStochasticOscillatorD(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create StochasticOscillatorKIndicator
    var sod = new StochasticOscillatorDIndicator(new StochasticOscillatorKIndicator(bts, num))
    indicators.add(sod)
    return sod
  }
  
  def createADL(): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    var adl = new volume.AccumulationDistributionIndicator(bts)
    indicators.add(adl)
    return adl
  }
  
  //BollingerBands
  def createBBL(num:Int){
    if(!helperInit){
      InitHelpers()
    }
    var sma = new SMAIndicator(closePrice, num)
    var sd = new statistics.StandardDeviationIndicator(closePrice, num)
    //Create BollingerBandsMiddle
    var bbm = new bollinger.BollingerBandsMiddleIndicator(sma)
    indicators.add(bbm)
    //Create BollingerBandsLower
    var bbl = new bollinger.BollingerBandsLowerIndicator(bbm, sd)
    indicators.add(bbl)
    //Create BollingerBandsUpper
    var bbu = new bollinger.BollingerBandsUpperIndicator(bbm, sd)
    indicators.add(bbu)
    //Create BollingerBandsWidth
    var bbw = new bollinger.BollingerBandWidthIndicator(bbu, bbm, bbl)
    indicators.add(bbw) 
    //Create BollingerBands%
    var bbp = new bollinger.PercentBIndicator(closePrice, num, Decimal.TWO)
    indicators.add(bbp)
  }
  
  def createVWAP(num:Int): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create VWAP
    var vwap = new volume.VWAPIndicator(bts, num)
    indicators.add(vwap)
    return vwap
  }
  
  def createPVI(): Indicator[_] = {
    if(!helperInit){
      InitHelpers()
    }
    //Create PVI
    var pvi = new volume.PVIIndicator(bts)
    indicators.add(pvi)
    return pvi
  }
  
  def InitHelpers(){
    closePrice = new ClosePriceIndicator(bts)
    typicalPrice = new TypicalPriceIndicator(bts)
    priceVariation = new PriceVariationIndicator(bts)
    helperInit = true
  }
  
  def writeTrendIndicators(trends:ListBuffer[Float], startAt:Int = 0, endAt:Int = Int.MaxValue, usableTrends:ListBuffer[Int], ratio:Float = 0.7f){
    var idx = 0
    var str = ""
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"))
    var maxidx = trends.length - 1
    if(maxidx > endAt) maxidx = endAt
    var breaker = ((maxidx-startAt) * ratio).toInt + startAt
    var up = 0
    var down = 0
    var side = 0
    
    for(i <- 0 to trends.length -1){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(i >= startAt && i <= breaker && trends(i).toInt != 0){
        str = trends(i).toInt.toString()
        if(trends(i).toInt == 0) side = side + 1
        if(trends(i).toInt == 1) up = up + 1
        if(trends(i).toInt == -1) down = down + 1
        var n = 0
        for(e <- indicators.toArray()){
         n = n + 1
         if(n == 1 || n == 2){
          str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i).toString().toDouble/bts.getBar(i).getClosePrice.toDouble()
        }else{
          str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i) 
        }
         //str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i)
//          var number = e.asInstanceOf[Indicator[_]].getValue(i).toString() 
//          var bd = new java.math.BigDecimal(number)
//          
//          str = str + " " + n + ":" + bd.round(new java.math.MathContext(5)).doubleValue()
        }
        str = str + "\n"
        println("Item: " + i + "\tDate:" + bts.getBar(i).getBeginTime.toString() + "\t" + str)
        writer.write(str)
      }
      
    }
    writer.close()
    
    println("\nTrain file => " + up + " up trends and " + down + " down trends.\n")
    up = 0
    down = 0
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"))
    for(i <- 0 to trends.length -1){
    //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(i >= breaker +1 && i <= maxidx && trends(i).toInt != 0){
        str = trends(i).toInt.toString()
        if(trends(i).toInt == 0) side = side + 1
        if(trends(i).toInt == 1) up = up + 1
        if(trends(i).toInt == -1) down = down + 1
        var n = 0
        for(e <- indicators.toArray()){
          n = n + 1
          if(n == 1 || n == 2){
          str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i).toString().toDouble/bts.getBar(i).getClosePrice.toDouble()
        }else{
          str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i) 
        }
          //str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i)
//          var number = e.asInstanceOf[Indicator[_]].getValue(i).toString() 
//          var bd = new java.math.BigDecimal(number)
//          
//          str = str + " " + n + ":" + bd.round(new java.math.MathContext(5)).doubleValue()
        }
        str = str + "\n"
        println("Item: " + i + "\t" + str)
        writer.write(str)
      }
      
    }
    writer.close()
    
    println("\nTest file => " + up + " up trends and " + down + " down trends.\n")
    
    //println("Writing to file " + up +" up trends, " + down + " down trends and " + side + " side trends.")
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\data"))
    for(i <- startAt to maxidx){
    //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      
      str = trends(i).toInt.toString()
        
      var n = 0
      for(e <- indicators.toArray()){
        n = n + 1
        str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i)
      }
      str = str + "\n"
      writer.write(str)
    }
    writer.close()
    
    
  }
  //priceNum < startAt
  def writeTrendPrice(priceNum:Int, cs:CandleSeries, trends:ListBuffer[Float], startAt:Int = 0, usableTrends:ListBuffer[Int], ratio:Float = 0.7f, all:Boolean){
    var idx = 0
    var str = ""
    var candles = cs.CS
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"))
    var maxidx = trends.length - 1
    var breaker = ((maxidx-startAt) * ratio).toInt + startAt
    
    for(i <- startAt to breaker){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(all){
        str = trends(i).toInt.toString()
        
        var n = 0
        for(e <- i-priceNum to i){
          n = n + 1
          str = str + " " + n + ":" + candles(e).close/candles(i).close
        }
        str = str + "\n"
        writer.write(str)
      }
      
    }
    writer.close()
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"))
    for(i <- breaker to maxidx){
    //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(all){
        str = trends(i).toInt.toString()
        
        var n = 0
        for(e <- i-priceNum to i){
          n = n + 1
          str = str + " " + n + ":" + candles(e).close/candles(i).close
        }
        str = str + "\n"
        writer.write(str)
      }
      
    }
    writer.close()
    
  }
  
  def print(){
    var str = ""
    for(i <- 0 to bts.getBarCount -1){
      str = bts.getBar(i).getClosePrice.toString() + " "
      var n = 0
      for(e <- indicators.toArray()){
        n = n + 1
        if(n == 1 || n == 2){
          str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i).toString().toDouble/bts.getBar(i).getClosePrice.toDouble()
        }else{
          str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(i) 
        }
        
      }
      println(str)
    }
  }
  
//  def divideIndicator(){
//    var indicator = indicators.toArray().last
//    for(e <- 0 to bts.getBarCount){
//       var i = indicator.asInstanceOf[Indicator[_]].getValue(e).toString().toDouble / bts.getBar(e).getClosePrice.toDouble()
//       indicator.asInstanceOf[Indicator[_]].
//    }
//  }
  
}