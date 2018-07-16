package core.indicators

import core.CandleSeries

class RSI(var number:Int, var cs:CandleSeries){
  var values = scala.collection.mutable.ListBuffer.empty[Float]
  var candlesL = cs.CS
  var change:Float = 0
  var gain = scala.collection.mutable.ListBuffer.empty[Float]
  var loss = scala.collection.mutable.ListBuffer.empty[Float]
  var avggain = scala.collection.mutable.ListBuffer.empty[Float]
  var avgloss = scala.collection.mutable.ListBuffer.empty[Float]
  var rs = scala.collection.mutable.ListBuffer.empty[Float]
  var rsi = scala.collection.mutable.ListBuffer.empty[Int]
  
  def UpdateIndicator() = {
    val csInv = candlesL.reverse
    //Create change array
    var temp:Float = 0
    for(e <- csInv){
      change = e.close - temp
      //check if it is gain or loss
      if(change < 0){
        loss.append(change)
        gain.append(0)
      }else if(change > 0){
        loss.append(0)
        gain.append(change)
      }else{
        loss.append(0)
        gain.append(0)
      }
      temp = e.close
    }
    loss(0) = 0
    gain(0) = 0
    
    //Calculate avggain and avgloss
    var sumgain:Float = 0
    var sumloss:Float = 0
    for(i <- 0 to number-1){
      sumgain += gain(i)
      sumloss += loss(i)
      avgloss.append(0)
      avggain.append(0)
    }
    avgloss.append(sumloss/number)
    avggain.append(sumgain/number)
    
    for(i <- number + 1 to candlesL.length -1){
      avgloss.append(((number-1)*avgloss(i-1)+loss(i))/number)
      avggain.append(((number-1)*avggain(i-1)+gain(i))/number)
    }
    
    //Calculate RS and RSI
    for(i <- 0 to candlesL.length -1){
      rs.append(scala.math.abs(avggain(i) / avgloss(i)))
      rsi.append(100-(100.0/(1.0+rs(i))).toInt)
    }
    
  }
  
  def PrintIndicator() = {
    for(i <- 0 to  candlesL.length/2 -1){
      println(i + "\t\t"  +gain(i) + "\t\t"  + loss(i) + "\t\t"  + avggain(i) + "\t\t"  + avgloss(i) + "\t\t"  + rs(i) + "\t\t"  + rsi(i))
    }
  }
  
}