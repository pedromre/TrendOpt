package core

import java.io._
import org.ta4j.core._

class ManualEvaluator(cs:CandleSeries, lookahead:Int, name:String, future:Boolean = false, doEvaluation:Boolean = false) {
  var trends = scala.collection.mutable.ListBuffer.empty[Float]
  var evaluated = scala.collection.mutable.ListBuffer.empty[Int]
  var numDown = 0
  var numUp = 0
  var numSide = 0
  var noTrend = 2f
  var exit:Boolean = false
  
  def startEvaluator(){
    //Fill trends with no trends
    if(trends.isEmpty){
      for(e <- cs.CS) trends.append(2f)
    }
    //Load trends from file
    println("Loading trends...")
    loadTrends
    //If evaluation is selected
    if(doEvaluation){
      println("Starting evaluation...")
      while(!exit){
        evaluate
        println("Evaluated " + evaluated.length + " trends!")
      }
      println("Writing trends to file")
      writeTrends  
    }
  }
  
  def evaluate(){
    var rnd = scala.util.Random
    var num = rnd.nextInt(trends.length)
    if(num > lookahead && !evaluated.contains(num) && lookahead + num < trends.length -1){
      println("Evaluating index: " + num + " ..." )
      if(future){
        evaluateFutureTrend(num)
      }else{
        evaluateTrend(num)  
      }
      
    }
    
  }
  
  def evaluateTrend(i:Int){
    var chart = new CandlestickChart(cs.getSubset(i - lookahead, i))
    chart.setVisible(true)
    chart.setAlwaysOnTop(true)
    println("Trend? - " + cs.getCandleDate(i).toString())
    var input = scala.io.StdIn.readLine()
    println("A: " + input)
    chart.setVisible(false)
    if(input.equals("u")){
      trends(i) = 1f;
      evaluated.append(i)
    }else if(input.equals("d")){
      trends(i) = -1f;
      evaluated.append(i)
    }else if(input.equals("s")){
      trends(i) = 0f;
      evaluated.append(i)
    }else if(input.equals("e")){
      exit = true
    }
  }
  
  def evaluateFutureTrend(i:Int){
    var chart = new CandlestickChart(cs.getSubset(i, i + lookahead))
    chart.setVisible(true)
    chart.setAlwaysOnTop(true)
    println("Trend? - " + cs.getCandleDate(i).toString())
    var input = scala.io.StdIn.readLine()
    println("A: " + input)
    chart.setVisible(false)
    if(input.equals("u")){
      trends(i) = 1f;
      evaluated.append(i)
    }else if(input.equals("d")){
      trends(i) = -1f;
      evaluated.append(i)
    }else if(input.equals("s")){
      trends(i) = 0f;
      evaluated.append(i)
    }else if(input.equals("e")){
      exit = true
    }
  }
  
  def print(){
    for(e <- 0 to trends.length -1){
      if(trends(e).!=(2f)){
        println(e+":"+trends(e))
        var chart = new CandlestickChart(cs.getSubset(e - lookahead, e))
        chart.setVisible(true)
        chart.setAlwaysOnTop(true)
        Thread.sleep(3000)
        chart.setVisible(false)
      }
    }
  }
  
  def writeTrends(){
    var idx = 0
    var str = ""
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\trends"))
    var maxidx = trends.length - 1
    
    for(i <- evaluated){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(trends(i).!=(noTrend)){
        str = i + ":" + trends(i).toInt.toString() + "\n"
        writer.write(str)
      }
    }
    writer.close()
  }
  
  def loadTrends(){
    try{
      val bufferedSource = io.Source.fromFile("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\trends")
      for (line <- bufferedSource.getLines) {
          val cols = line.split(":").map(_.trim)
          if(cols.length == 2){
            trends(cols(0).toInt) = cols(1).toFloat
            evaluated.append(cols(0).toInt)
          }
      }
    }catch{
      case e: Exception => println(e.toString())
    }
    evaluated = scala.util.Random.shuffle(evaluated)
  }
  
   def writeTrendPrice(priceNum:Int, cs:CandleSeries, startAt:Int = 0, ratio:Float = 0.7f){
    var idx = 0
    var str = ""
    var candles = cs.CS
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"))
    var maxidx = trends.length - 1
    var breaker = ((maxidx-startAt) * ratio).toInt + startAt
    
    for(i <- startAt to breaker){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(evaluated.contains(i)){
        str = trends(i).toInt.toString()
        
        var n = 0
        for(e <- i-priceNum to i){
          n = n + 1
          str = str + " " + n + ":" + candles(e).close
        }
        str = str + "\n"
        println(str)
        writer.write(str)
      }
      
    }
    writer.close()
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"))
    for(i <- breaker to maxidx){
    //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      if(evaluated.contains(i)){
        str = trends(i).toInt.toString()
        
        var n = 0
        for(e <- i-priceNum to i){
          n = n + 1
          str = str + " " + n + ":" + candles(e).close
        }
        str = str + "\n"
        writer.write(str)
      }
      
    }
    writer.close()
    
  }
   
   def newWriteTrendPrice(priceNum:Int, cs:CandleSeries, startAt:Int = 0, ratio:Float = 0.7f){
    var idx = 0
    var str = ""
    var candles = cs.CS
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"))
    var maxidx = evaluated.length -1
    var breaker = (maxidx * ratio).toInt
    
    for(i <- 0 to breaker){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
        idx = evaluated(i)
        str = trends(idx).toInt.toString()
        if(idx-priceNum+1>=0){
          
        
          var n = 0
          for(e <- idx-priceNum +1 to idx){
            n = n + 1
            str = str + " " + n + ":" + candles(e).close/candles(idx).close
          }
          str = str + "\n"
          //println(idx +" => " + str)
          writer.write(str)
        }
        
      
      
    }
    writer.close()
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"))
    for(i <- breaker + 1 to maxidx){
    //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
        idx = evaluated(i)
        str = trends(idx).toInt.toString()
        if(idx-priceNum+1>=0){
           var n = 0
          for(e <- idx-priceNum+1 to idx){
            n = n + 1
            str = str + " " + n + ":" + candles(e).close/candles(idx).close
          }
          str = str + "\n"
          writer.write(str)
        }
        
      
      
    }
    writer.close()
    
//    for(k <- evaluated){
//      println(k + "->")
//    }
  }
   
   def newWriteTrendIndicator(priceNum:Int, cs:CandleSeries, startAt:Int = 0, ratio:Float = 0.7f, TSL:TimeSeriesLoader){
    var idx = 0
    var str = ""
    var candles = cs.CS
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"))
    var maxidx = evaluated.length -1
    var breaker = (maxidx * ratio).toInt
    
    for(i <- startAt to breaker){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
        idx = evaluated(i)
        str = trends(idx).toInt.toString()
        if(idx-priceNum+1>=0){
          
        
          var n = 0
          for(e <- TSL.indicators.toArray()){
            n = n + 1
            str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(idx)
          }
          str = str + "\n"
          //println(idx +" => " + str)
          writer.write(str)
        }
        
      
      
    }
    writer.close()
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"))
    for(i <- breaker + 1 to maxidx){
    //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
        idx = evaluated(i)
        str = trends(idx).toInt.toString()
        if(idx-priceNum+1>=0){
           var n = 0
          for(e <- TSL.indicators.toArray()){
            n = n + 1
            str = str + " " + n + ":" + e.asInstanceOf[Indicator[_]].getValue(idx)
          }
          str = str + "\n"
          writer.write(str)
        }
        
      
      
    }
    writer.close()
    
//    for(k <- evaluated){
//      println(k + "->")
//    }
  }
  
  
}