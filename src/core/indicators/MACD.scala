package core.indicators

import core.CandleSeries


class MACD(var cs:CandleSeries, var a:Int=12, var b:Int=26, var c:Int=9) {
  var MACDline = scala.collection.mutable.ListBuffer.empty[Float]
  var SignalLine = scala.collection.mutable.ListBuffer.empty[Float]
  var MACDHistogram = scala.collection.mutable.ListBuffer.empty[Float]
  
  def UpdateIndicator(){
    var EMAa = new EMA(a, cs)
    var EMAb = new EMA(b, cs)
    EMAa.UpdateIndicator()
    EMAb.UpdateIndicator()
    //MACDline = EMA12-EMA26
    for(i <- 0 to cs.CS.length-1){
      MACDline.append(EMAa.values(i) - EMAb.values(i))
    }
    for(i <- 0 to b-2){
      MACDline(i) = 0
    }
    //SignalLine
    var temp = new EMAlist(c, MACDline, b-1)
    temp.UpdateIndicator()
    
    SignalLine = temp.values
    
    //MACDHistogram = MACDline - SignalLine
    for(i <- 0 to cs.CS.length-1){
      MACDHistogram.append(MACDline(i) - SignalLine(i))
    }
    
    for(i <- 0  to b+c-3 ){
      MACDHistogram(i) = 0
    }
    
  }
  
  def PrintIndicator() = {
    for(i <- 0 to cs.CS.length/2-1){
      println("MACDline: " + MACDline(i) + "\t\tSignalLine: " + SignalLine(i) + "\t\tMACDHistogram: " + MACDHistogram(i))
    }
  } 
  
}