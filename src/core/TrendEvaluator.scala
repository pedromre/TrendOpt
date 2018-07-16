package core
import scala.util.control.Breaks._
import java.io._

class TrendEvaluator(var candleSeries: CandleSeries) {
  
  var trends = scala.collection.mutable.ListBuffer.empty[Float]
  var numDown = 0
  var numUp = 0
  var numSide = 0
  var upTrends = scala.collection.mutable.ListBuffer.empty[Int]
  var downTrends = scala.collection.mutable.ListBuffer.empty[Int] 
  var sideTrends = scala.collection.mutable.ListBuffer.empty[Int]
  var usableTrends = scala.collection.mutable.ListBuffer.empty[Int]

  
  def separateTrends(startingAt:Int = 0){
    //Separate trends
    for(i <- startingAt to trends.length-1){
      if(trends(i) == 1.0) upTrends.append(i)
      if(trends(i) == 0.0) sideTrends.append(i)
      if(trends(i) == -1.0) downTrends.append(i) 
    }
    //Create a balanced number of trends and put them on usableTrends
    var size = math.min(sideTrends.length, math.min(upTrends.length, downTrends.length))
    upTrends = util.Random.shuffle(upTrends)
    downTrends = util.Random.shuffle(downTrends)
    sideTrends = util.Random.shuffle(sideTrends)
    for(i <- 0 to size - 1){
      usableTrends.append(upTrends(i))
      usableTrends.append(downTrends(i))
      //usableTrends.append(sideTrends(i))
    }
    
  }
  
  def classifyTrendUsingAvgValue(lookahead:Int, candlesBufferSize:Int, changeThreshold:Float){
    trends = scala.collection.mutable.ListBuffer.empty[Float]
    var candles = candleSeries.CS
    for(i <- 0 to candles.length-1){
       if(i < candlesBufferSize){
         trends.append(Float.NaN)
       }else{
         //var currentPrice = candles(i).close
         var change = 0
         var sum = 0f
         for(e <- i - lookahead + 1 to i){
           if(i > 0){
             sum = sum + candles(e).close
           }
         }
         
         var futureprice = sum / (lookahead).toFloat
         var pastprice = candles(i-lookahead).close
         
         if(pastprice*(1f-changeThreshold) > futureprice){
                 change = -1
                 numDown += 1
                 
         }
         if(pastprice*(1f+changeThreshold) < futureprice){
                 change = 1 
                 numUp += 1
                 
         }
         
         if(change == 0) numSide += 1
         
         //Add trend to trends
         trends.append(change)
       }
    }
  }
  
  def calculateTrendUsingAvgValue(lookahead:Int, candlesBufferSize:Int, changeThreshold:Float){
    trends = scala.collection.mutable.ListBuffer.empty[Float]
    var candles = candleSeries.CS
    for(i <- 0 to candles.length-1){
       if(i < candlesBufferSize){
         trends.append(Float.NaN)
       }else{
         var currentPrice = candles(i).close
         var change = 0
         
         //Search for next trend
//         breakable {
//           for(e <- i to i+lookahead){
//             if(i+lookahead<candles.length){
//               var v1 = currentPrice*(1f-changeThreshold)
//               var v2 = candles(e).low
//               var v3 = currentPrice*(1f+changeThreshold)
//               var v4 = candles(e).high
//               
//               if(currentPrice*(1f-changeThreshold) > candles(e).low){
//                 change = -1
//                 numDown += 1
//                 break
//               }
//               if(currentPrice*(1f+changeThreshold) < candles(e).high){
//                 change = 1 
//                 numUp += 1
//                 break
//               }
//             }
//           }
//         }
         var sum = 0f
         for(e <- i +1 to i+lookahead){
           if(i+lookahead < candles.length){
             sum = sum + candles(e).close
           }
         }
         
         var futureprice = sum / (lookahead).toFloat
         
         if(currentPrice*(1f-changeThreshold) > futureprice){
                 change = -1
                 numDown += 1
                 
         }
         if(currentPrice*(1f+changeThreshold) < futureprice){
                 change = 1 
                 numUp += 1
                 
         }
         
         if(change == 0) numSide += 1
         
         //Add trend to trends
         trends.append(change)
       }
    }
  
  }
  
  def calculateTrendUsingClosingValue(lookahead:Int, candlesBufferSize:Int, changeThreshold:Float){
    trends = scala.collection.mutable.ListBuffer.empty[Float]
    var candles = candleSeries.CS
    for(i <- 0 to candles.length-1){
       if(i < candlesBufferSize){
         trends.append(Float.NaN)
       }else{
         var currentPrice = candles(i).close
         var change = 0
         var futureprice = 0f
         
         if(i+lookahead < candles.length){
           futureprice = candles(i+lookahead).close
         }else{
           futureprice = candles.last.close
         }
         
         
         
         if(currentPrice*(1f-changeThreshold) > futureprice){
                 change = -1
                 numDown += 1
                 
         }
         if(currentPrice*(1f+changeThreshold) < futureprice){
                 change = 1 
                 numUp += 1
                 
         }
         
         if(change == 0) numSide += 1
         
         //Add trend to trends
         trends.append(change)
       }
    }
  
  }
  
  def calculateTwoTrendUsingClosingValue(lookahead:Int, candlesBufferSize:Int){
    trends = scala.collection.mutable.ListBuffer.empty[Float]
    var candles = candleSeries.CS
    for(i <- 0 to candles.length-1){
       if(i < candlesBufferSize){
         trends.append(Float.NaN)
       }else{
         var currentPrice = candles(i).close
         var change:Int = 0
         var futureprice = 0f
         
         if(i+lookahead < candles.length){
           futureprice = candles(i+lookahead).close
         }else{
           futureprice = candles.last.close
         }
         
         
         
         if(currentPrice > futureprice){
                 change = -1
                 numDown += 1
                 
         }
         if(currentPrice < futureprice){
                 change = 1 
                 numUp += 1
                 
         }
         
         if(change == 0) numSide += 1
         
         //Add trend to trends
         trends.append(change)
       }
    }
  
  }
  
  def writeTrendsToFile(buffer:Int){
    var candles = candleSeries.CS
    var idx = 0
    var str = ""
    
    var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"))
    for(i <- buffer to ((trends.length-1)*0.7).toInt){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      var trend = 10
      if(trends(i) == -1) trend = 20
      if(trends(i) == 1) trend = 30
      str = trend+""
      for(e <- 1 to buffer){
        idx = i - e
        str = str + " " + e + ":" + candles(idx).close
      }
      str = str + "\n"
      writer.write(str)
    }
    writer.close()
    
    writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"))
    for(i <- ((trends.length-1)*0.7).toInt + 1 to trends.length-1){
      //var pastvalues = scala.collection.mutable.ListBuffer.empty[Float]
      var trend = 10
      if(trends(i) == -1) trend = 20
      if(trends(i) == 1) trend = 30
      str = trend+""
      for(e <- 1 to buffer){
        idx = i - e
        str = str + " " + e + ":" + candles(idx).close
      }
      str = str + "\n"
      writer.write(str)
    }
    writer.close()
    
  }
  
  def printTrends(){
//    for(e <- trends){
//      println("Trend:" + e)
//    }
    println("Up: " + numUp + "\tDown: " + numDown + "\tSide: " + numSide)
  }
  
}