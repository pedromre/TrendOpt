package core

import java.util._
import java.io._
import java.lang._
import scala.collection.mutable.ListBuffer

class SVM {
  var str = ""
  var gamma = Array( 0.001, 0.01, 0.1, 1, 10,50,100,500,1000)
  var C = Array(0.1,0.5,1,5,10,50,100,500,1000)
  var resultsMap = scala.collection.mutable.HashMap.empty[(Double,Double),scala.collection.mutable.ListBuffer[Int]]
  var list = new java.util.HashMap[(Double, Double), ListBuffer[Double]]
  def train(gamma:Double, Cval:Double){
    var result = new Array[String](7)
    result(0) = "-s"
    result(1) = "0"
    result(2) = "-g"
    result(3) = gamma.toString()
    result(4) = "-c"
    result(5) = Cval.toString()
    //result(4) = "-w0"
    //result(5) = "1"
    //result(6) = "-w-1"
    //result(7) = "3"
    //result(8) = "-w1"
    //result(9) = "3"
    result(6) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"
  
    var svm = svm_train.main(result)
  }
  
  def trainCross(gamma:Double, Cval:Double, crossk:Int, method:String, Dval:Int = 3){
    
    if(method.equals("rbf")){
      var result = new Array[String](9)
      result(0) = "-t"
      result(1) = "2"
      result(2) = "-g"
      result(3) = gamma.toString()
      result(4) = "-c"
      result(5) = Cval.toString()
      result(6) = "-v"
      result(7) = crossk.toString()
      result(8) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"
      var svm = svm_train.main(result)
    }else if(method.equals("poly")){
      var result = new Array[String](9)
      result(0) = "-t"
      result(1) = "1"
      result(2) = "-d"
      result(3) = Dval.toString()
      result(4) = "-c"
      result(5) = Cval.toString()
      result(6) = "-v"
      result(7) = crossk.toString()
      result(8) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"
      var svm = svm_train.main(result)
    }else if(method.equals("linear")){
      var result = new Array[String](7)
      result(0) = "-t"
      result(1) = "0"
      result(2) = "-c"
      result(3) = Cval.toString()
      result(4) = "-v"
      result(5) = crossk.toString()
      result(6) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train"
      var svm = svm_train.main(result)
    }
    
  }
  
  def test(){
    var result = new Array[String](3)
    result(0) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test"   
    result(1) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\train.model"
    result(2) = "D:\\eclipse-workspace\\TrendPrediction\\src\\core\\out"
    
    var svm = svm_predict.main(result)
    
    //var t = svm_predict.getTarget()
    
    //var p = svm_predict.getPredicted()
    
    //println("t: " + t.size() + "\tp:" + p.size())
    
//    for (i <- 0 to t.size() - 1){
//      println("t: " + t.get(i) + "\tp:" + p.get(i))
//    }
    
  }
  
//  def score(p:ArrayList[Double], t:ArrayList[Double]){
//    var matrix = new Integer[10]
//    var i = 0
//    var j = 0
//    for(e <- 0 to t.size() -1){
//      var exp = t.get(e)
//      var pred = t.get(e)
//      if(exp == 10.0d){
//        i = 1
//      }else if(exp == 20.0d){
//        i = 2
//      }else{
//        i = 0
//      }
//      if(pred == 10.0d){
//        i = 1
//      }else if(pred == 20.0d){
//        i = 2
//      }else{
//        i = 0
//      }
//      
//      matrix[i][j] += 1
//      
//    }
//  }
  

  def gridSearch(){
    
    var max:Double = 0
    var parameters:(Double,Double) = (0,0)
    //'C': [1, 10, 100, 1000], 'gamma': [0.001, 0.0001]
    
    for(gammaval <- gamma){
      for(Cval <- C){
        train(gammaval, Cval)
        test()
        var accuracy = analyzeResults
        if(accuracy > max){
          max = accuracy
          println("New max: " + max)
          parameters = (gammaval, Cval)
        }
        str = str + "Accuracy: " + accuracy + "\tParam: " + gammaval + " and " + Cval +"\n"
        println("\t\t\t\tAccuracy: " + accuracy + "\tParam: " + gammaval + " and " + Cval)
      }
    }
    println(str)
    println("Final parameters: " + parameters + "\tAccuracy: " + max)
    
  }
  
  def gridSearchCross(fold:Int = 5, method:String = "rbf"):(Double, Double)={
    var max:Double = 0
    var parameters:(Double,Double) = (0,0)
    //'C': [1, 10, 100, 1000], 'gamma': [0.001, 0.0001]
    if(method.equals("poly") || method.equals("rbf")){
      for(gammaval <- gamma){
        for(Cval <- C){
          trainCross(gammaval, Cval, fold, method)
          
          var accuracy = svm_train.getCVA
          try{
            list.get((Cval,gammaval)).append(accuracy)
          }catch
          {
            case _: Throwable => println("Couldn't perform statistics on grid search method")
          }
          
          if(accuracy > max){
            max = accuracy
            println("New max: " + max)
            parameters = (gammaval, Cval)
          }
          str = str + "Accuracy: " + accuracy + "\tParam: " + gammaval + " and " + Cval +"\n"
          println("\t\t\t\tAccuracy: " + accuracy + "\tParam: " + gammaval + " and " + Cval)
        }
      }
    }else if(method.equals("linear")){
      
        for(Cval <- C){
          trainCross(0, Cval, fold, method)
          
          var accuracy = svm_train.getCVA
          try{
            list.get((Cval,0)).append(accuracy)
          }catch
          {
            case _: Throwable => println("Couldn't perform statistics on grid search method")
          }
          
          if(accuracy > max){
            max = accuracy
            println("New max: " + max)
            parameters = (0, Cval)
          }
          str = str + "Accuracy: " + accuracy + "\tParam: " + 0 + " and " + Cval +"\n"
          println("\t\t\t\tAccuracy: " + accuracy + "\tParam: " + 0 + " and " + Cval)
        }
      
    }
    
    println(str)
    println("Final parameters: " + parameters + "\tAccuracy: " + max)
    return parameters
  }
  
  def analyzeResults():Double ={
     var testTrends = ListBuffer.empty[Integer]
     var predictedTrends = ListBuffer.empty[Integer]
     //Matrix (accurate, predicted) a- 1 b- 0 c- -1
     var aa = 0 
     var ab = 0
     var ac = 0
     var ba = 0
     var bb = 0
     var bc = 0
     var ca = 0
     var cb = 0
     var cc = 0
     
     var bufferedSource = io.Source.fromFile("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\test")
      
     for (line <- bufferedSource.getLines) {
          val cols = line.split(" ").map(_.trim)
          cols(0)
          testTrends.append(cols(0).toFloat.toInt)
     }
     bufferedSource.close
     
     bufferedSource = io.Source.fromFile("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\out")
      
     for (line <- bufferedSource.getLines) {
          val cols = line.split(" ").map(_.trim)
          cols(0)
          predictedTrends.append(cols(0).toFloat.toInt)
     }
     bufferedSource.close
     
     println("Trends: " + testTrends.length + "\tPredicted: " + predictedTrends.length)
     
     for(i <- 0 to testTrends.length -1){
       if(testTrends(i) == 1){
         //true : a
         if(predictedTrends(i) == 1){
           aa = aa + 1
         }else if(predictedTrends(i) == 0){
           ab = ab + 1
         }else if(predictedTrends(i) == -1){
           ac = ac + 1
         }
       }else if(testTrends(i) == 0){
         //true : b
         if(predictedTrends(i) == 1){
           ba = ba + 1
         }else if(predictedTrends(i) == 0){
           bb = bb + 1
         }else if(predictedTrends(i) == -1){
           bc = bc + 1
         }
       }else if(testTrends(i) == -1){
         //true : c
         if(predictedTrends(i) == 1){
           ca = ca + 1
         }else if(predictedTrends(i) == 0){
           cb = cb + 1
         }else if(predictedTrends(i) == -1){
           cc = cc + 1
         }
       }
     }
     
     println("For label up (1) -> TP:" + aa + "\tFP:" + (ba + ca) + "\tFN:" + (ab + ac) + "\tTotal:" + (aa+ba+ca))
     println("For label side (0) -> TP:" + bb + "\tFP:" + (ab + cb) + "\tFN:" + (ba + bc) + "\tTotal:" + (ab+bb+cb))
     println("For label down (-1) -> TP:" + cc + "\tFP:" + (ac + bc) + "\tFN:" + (ca + cb) + "\tTotal:" + (ac+bc+cc))
     println()
     var pa = (aa.toFloat/(aa+ba+ca).toFloat)
     var pb = (bb.toFloat/(ab+bb+cb).toFloat)
     var pc = (cc.toFloat/(ac+bc+cc).toFloat)
     var ra = (aa.toFloat/(aa+ab+ac).toFloat)
     var rb = (bb.toFloat/(ba+bb+bc).toFloat)
     var rc = (cc.toFloat/(ca+cb+cc).toFloat)
     str = str + "For label up (1) -> Precision: " + pa + "\tTotal: " + (aa+ba+ca) + "\n"
     str = str + "For label up (0) -> Precision: " + pb + "\tTotal: " + (ab+bb+cb) + "\n"
     str = str + "For label up (-1) -> Precision: " + pc + "\tTotal: " + (ac+bc+cc) + "\n"
     println("For label up (1) -> Precision:" + pa + "\tRecall:" + ra)
     println("For label side (0) -> Precision:" + pb + "\tRecall:" + rb)
     println("For label down (-1) -> Precision:" + pc + "\tRecall:" + rc)
     println("Accuracy : " +  ((aa+bb+cc).toFloat/(aa+ab+ac+ba+bb+bc+ca+cb+cc).toFloat) + "\tAvg Precision: " + ((pa+pb+pc)/3f) + "\tAvg Recall: " + ((ra+rb+rc)/3f))
     var p = (pa + pc)/2
     var r = (ra + rc)/2
     var f1 = 2*((p*r)/(p+r))
     var acc2 = (aa + cc).toFloat / (aa + cc + ac + ca).toFloat
     return (acc2).toDouble
     //return (((pa+pb+pc)/3f)*0.5f + ((ra+rb+rc)/3f)*0.5f).toDouble
  }
  
  def grid(){
    var method = "linear"
    
    if(method.equals("linear")){
      for(c <- C){
        list.put((c,0), ListBuffer.empty[Double])
      }
    }else{
      //Initialize list
      for(c <- C){
        for(g <- gamma){
          list.put((c,g), ListBuffer.empty[Double])
        }
      }  
    }
    
    
    //GridSearch
    for(g <- 1 to 50 ) gridSearchCross(5,method)
    
    println("Writing to file...")
    
    if(method.equals("linear")){
      var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\stats\\" + method))
      var str = ""
      for(c <- C){
        
          str = c.toString() 
          var values = list.get((c,0))
          for(v <- values){
            str = str + "," + v
          }
          str = str + "\n"
          writer.write(str)
        
      }
      writer.close() 
    }else{
      var writer = new PrintWriter(new File("D:\\eclipse-workspace\\TrendPrediction\\src\\core\\stats\\" + method))
      var str = ""
      for(c <- C){
        for(g <- gamma){
          str = c.toString() + "," + g.toString() 
          var values = list.get((c,g))
          for(v <- values){
            str = str + "," + v
          }
          str = str + "\n"
          writer.write(str)
        }
      }
      writer.close() 
    }
    
    
    println("Done writing!")
    
  }
  
  
}