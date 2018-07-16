package core

import core.indicators._
import java.text.SimpleDateFormat
import java.util.GregorianCalendar
import java.sql.Date
import org.jgap.Genotype
import org.jgap.IChromosome

//import java.*

class Candle(var high:Float = 0, var low:Float = 0, var open:Float = 0, var close:Float = 0,
     var time:GregorianCalendar= new GregorianCalendar(), var timeStamp:String = "", var volume:Long = 0){

  def color(): Int = {
    if(open==close) 1 else 0
  }

  override def toString = "high: " + high + " low: " + low + " open: " + open + " close: " + close + " volume: " + volume + " time: " + timeStamp

}


class CandleSeries(name:String = "Stock's Name"){
  
  var CS = scala.collection.mutable.ListBuffer.empty[Candle]
  
  def getCandleClose(i:Int):Double ={
    return CS(i).close.toDouble
  }
  
  def getCandleOpen(i:Int):Double ={
    return CS(i).open.toDouble
  }
  
  def getCandleHigh(i:Int):Double ={
    return CS(i).high.toDouble
  }
  
  def getCandleLow(i:Int):Double ={
    return CS(i).low.toDouble
  }
  
  def getCandleVolume(i:Int):Double ={
    return CS(i).volume.toDouble
  }
  
  def getCandleDate(i:Int):java.util.Date ={
    return CS(i).time.getTime
  }
  
  def printCS = {
    for(e <- CS){
      println(e.toString())
    }
  }
  
  def getSubset(start:Int = 0, end:Int = 0) : CandleSeries = {
    var subset = new CandleSeries 
    for(e<-start to end){
      subset.CS.append(CS(e))
    }
    return subset
  }
  
  def getSubset(start:GregorianCalendar, end:GregorianCalendar) : CandleSeries = {
    var subset = new CandleSeries 
    for(e <- CS){
      if(start.getTime.getTime <= e.time.getTime.getTime && end.getTime.getTime >= e.time.getTime.getTime)
        subset.CS.append(e)
    }
    return subset
  }
  
  def changeClose(array:Array[Float]){
    var csInv = CS
    var i = 0
    for(e <- array){
      csInv(i).close = e
      i += 1
    }
    CS = csInv
  }
  
  def changeVolume(array:Array[Long]){
    var csInv = CS
    var i = 0
    for(e <- array){
      csInv(i).volume = e
      i += 1
    }
    CS = csInv
  }
  
  def getLU():(Float, Float) = {
    var lower = Float.MaxValue
    var upper = Float.MinValue
    for(e <- CS){
      if(e.close > upper) upper = e.close
      if(e.close < lower) lower = e.close
    }
    return(upper, lower)
  }
  
  def getSize():Int = {
    return CS.length
  }
  
}


object createlist {
  def main(args: Array[String]){
    println("Hello, world!")
 
    

    def ReadFile(path:String) : CandleSeries = {
      var CS = new CandleSeries()
      val sdf = new SimpleDateFormat("yyyy MMM dd HH:mm:ss");
      
      val bufferedSource = io.Source.fromFile("D:\\data\\Stocks\\aapl.us.txt")
      for (line <- bufferedSource.getLines) {
          val cols = line.split(",").map(_.trim)
          if(cols.length == 7 & cols(0) != "Date"){
            //println("date: " + cols(0) + " open: " + cols(1) + " high: " + cols(2) + " low: " + cols(3) + " close: " + cols(4))
            var date = DateToGregorianCalendarDate(cols(0))
            var temp = new Candle(cols(2).toFloat,cols(3).toFloat,cols(1).toFloat,cols(4).toFloat, date, cols(0), cols(5).toLong)
            CS.CS.append(temp)
          }
          //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
      }
      bufferedSource.close 
      //CS.printCS
      return CS
    }
    
    def DateToGregorianCalendarDate(date:String) : GregorianCalendar = {
      //var format = new java.text.SimpleDateFormat("yyyy MMM dd HH:mm:ss")
      var d = date.split("-")
      var year = d(0).toInt
      var month = d(1).toInt - 1
      var dayOfMonth = d(2).toInt
      var hourOfDay = 0
      var minute = 0
      return new java.util.GregorianCalendar(year, month, dayOfMonth, hourOfDay, minute)
    }
    

      var CS = ReadFile("lol");
      //CS.printCS


        
        var trends = new TrendEvaluator(CS)
        var startAt = CS.CS.length - 5000
        var lookahead = 1
        //trends.calculateTrendUsingAvgValue(7, startAt, 0.015f)
        //trends.classifyTrendUsingAvgValue(lookahead, startAt, 0.03f)
        //trends.calculateTwoTrendUsingClosingValue(lookahead, 7)
        //trends.printTrends()
        //trends.separateTrends(0) //ignore side trends
        var tsl = new TimeSeriesLoader
        tsl.createBTS(CS, "no name")
        
        //Create subset
//        var newCS = CS.getSubset(new GregorianCalendar(2006, 0, 1, 1, 1), new GregorianCalendar(2007, 0, 1, 1, 1))
//        println("newCS length: " + newCS.CS.length)
//        for(i <- 0 to 4){
//          var candleId = CS.CS.indexOf(newCS.CS(i))
//          println("Item: " + candleId + "\tTrend: " + trends.trends(candleId))
//        }
        
        //Create indicators
       var sma5 = tsl.createSMA(5)
       var sma20 = tsl.createSMA(20)
       var sma30 = tsl.createSMA(30)
       var sma100 = tsl.createSMA(100)
       var rsi14 =  tsl.createRSI(14)
       
      
      var strat = new Strategy(CS, "SMA Test", 100000)
      //strat.setEntryRule(tsl.convertTStoLB(sma5), "crossover", tsl.convertTStoLB(sma30))
//      strat.setEntryRuleTestUnder(tsl.convertTStoLB(rsi14), 30f)
//      strat.setExitRuleTestOver(tsl.convertTStoLB(rsi14), 70f)
      //strat.setStopLoss(5)
      //strat.setStopProfit(10)
      //strat.setExitRule(tsl.convertTStoLB(sma30), "crossover", tsl.convertTStoLB(sma5))
      //strat.simulateStrategy(CS.CS.length - 600, CS.CS.length-1)
      
      //strat.getHistoric(CS.CS.length - 600)
      
      System.out.println("Starting Genetic Algorithm")
      
      var GA = new GeneticAlgorithm(CS, tsl, CS.CS.length-201, CS.CS.length-1)
      var population = GA.configureJGAP() 
      val t1 = System.nanoTime
      var bestsol:IChromosome = null
      for(i <- 0 to 30) {
  			population.evolve();
  			bestsol = population.getFittestChromosome();
  			System.out.println("Best chromosome: " + bestsol.getGene(0) + "|" + bestsol.getGene(1) + "|" );
  			System.out.println("Fitness: " + bestsol.getFitnessValue());
  			
  		}
      
      System.out.println("Best chromosome: " + bestsol.getGene(0) + "|" + bestsol.getGene(1) + "|" + bestsol.getGene(2) + "|" + bestsol.getGene(3));
  		System.out.println("Fitness: " + bestsol.getFitnessValue());
  		strat.simulateStrategy(CS.CS.length - 201, CS.CS.length-1)
  		var up = 5
  		var down = 70
  		println("up:"+up+"down:"+down)
  		strat.setEntryRuleTestUnder(tsl.convertTStoLB(rsi14), down)
      strat.setExitRuleTestOver(tsl.convertTStoLB(rsi14), up)
      strat.setEntryRule(tsl.convertTStoLB(tsl.createSMA(20)), "", tsl.convertTStoLB(tsl.createSMA(40)))
		  strat.setExitRule(tsl.convertTStoLB(tsl.createSMA(40)), "", tsl.convertTStoLB(tsl.createSMA(20)))
      strat.simulateStrategy(CS.CS.length-201, CS.CS.length-1)
      println(strat.getROI())
      strat.getHistoric(0)
  		System.out.println("Buy&Hold: " + strat.getBandH());
  		
      val duration = (System.nanoTime - t1) / 1e9d
  		
      println("Time: " + duration + "\tCandles: " + CS.CS.length)
       
//        //tsl.createSMA(10)
//        tsl.createSMA(30)
//        //tsl.createSMA(50)
//        //tsl.createSMA(100)
//        
        tsl.createEMA(20)
        tsl.createEMA(100)
       
//        //tsl.createEMA(10)
//        tsl.createEMA(20)
//        
//        
       tsl.createStochasticOscillatorD(14) 
       tsl.createStochasticOscillatorK(14)
       tsl.createWilliamsR(14)
//        
//        
//        //tsl.createATR(5)
//        tsl.createATR(14)
//        //tsl.createATR(20)
//        
//        //tsl.createRSI(2)
//        tsl.createRSI(5)
       tsl.createRSI(14)
       
//        //tsl.createRSI(20)
//        
       tsl.createCCI(20)
//        //tsl.createCCI(20)
//        
//        //tsl.createROC(5)
       tsl.createROC(20)
//        
//        
        tsl.createBBL(20)
        
        //tsl.createVWAP(10) //Not suited for daily candles
        //startAt = CS.CS.indexOf(newCS.CS(0))
        //var finishAt = CS.CS.indexOf(newCS.CS.last)
        //println("finishAt: " + finishAt + "\tstartAt: " + startAt)
        //println("usable: " + trends.usableTrends.length)
        //tsl.writeTrendIndicators(trends.trends, startAt, finishAt, trends.usableTrends, 0.5f)
        //tsl.writeTrendPrice(CS, trends.trends, 20, trends.usableTrends)
        
        //tsl.writeTrendPrice(7 , CS, trends.trends, startAt+1, trends.usableTrends, 0.7f, true)
//      strat.exportToCVS(sma10.values, trends.trends)
//      //trends.writeTrendsToFile(100)
        var svm = new SVM
//      //var (max,min) = CS.getLU()
//      //svm.scale(max, min)
        
        //Rever criação de trends! Funcionamento do SVM está correcto
        
        
        //svm.gridSearch()
        //svm.gridSearchCross(5)
        //svm.train(50, 1)
        //svm.test()
        //svm.analyzeResults()
        //tsl.print()

        //var chart = new CandlestickChart(CS.getSubset(CS.CS.length - 100, CS.CS.length-1)).setVisible(true)
        
        //Create manual evaluator with evaluation mode off
        var me = new ManualEvaluator(CS,100,"appl", false, true)
        //Start evaluation
        //me.startEvaluator()
        //Write trends to file using normalized closing prices as features
        //me.newWriteTrendIndicator(100, CS, 100 + 1, 0.6f, tsl)
        
        //var parameters = svm.gridSearchCross(5)
        //println("Parameter 1: " + parameters._1 + "\tParameter 2: "+ parameters._2)
        //Train with param1 = gamma and param2 = C
        //svm.train(parameters._1, parameters._2)
        //Test with testing data
        //svm.test()
        //Analyze results
        //svm.analyzeResults()
        
        //svm.scale()
        //svm.trainCross(0.002, 1, 6)
        
////    rsi.UpdateIndicator()
////    rsi.PrintIndicator()
//      
//      var load  = new TimeSeriesLoader
//      load.create(CS, "apple")
  }
}
