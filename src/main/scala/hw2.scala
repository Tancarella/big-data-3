import scala.io.Source
import scala.collection._
import java.io._
import scala.math._
import Array._
import scala.util.matching.Regex

object hw2 extends App {
  var p = new PageRank2("AmanitaGraph.txt")
  p.getPR(1.0, 0.1)
  p.getPR(1.0, 0.01)
  p.getPR(1.0, 0.0000001)
  p.getPR(0.99, 0.1)
  p.getPR(0.99, 0.01)
  p.getPR(0.99, 0.0000001)
  p.getPR(0.95, 0.1)
  p.getPR(0.95, 0.01)
  p.getPR(0.95, 0.0000001)
  p.getPR(0.8, 0.1)
  p.getPR(0.8, 0.01)
  p.getPR(0.8, 0.0000001)
  p.mushroom(0.9, 0.000001)

}

class PageRank2(file: String){

  def makeMatrix(): mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] = {
    var matrix: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] = mutable.ArrayBuffer() //matrix M to calculate PageRank

    for(i<-1 to 8){
      matrix.append(mutable.ArrayBuffer())
    }

    matrix(0).append(1.0/6.0, 0.0, 1.0/6.0, 1.0/6.0, 1.0/6.0, 1.0/6.0, 1.0/6.0, 0.0)
    matrix(1).append(1.0/6.0, 0.0, 1.0/6.0, 1.0/6.0, 0.0, 1.0/6.0, 1.0/6.0, 1.0/6.0)
    matrix(2).append(1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 0.0, 1.0/7.0)
    matrix(3).append(0.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0)
    matrix(4).append(1.0/7.0, 0.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0)
    matrix(5).append(1.0/7.0, 0.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0)
    matrix(6).append(1.0/7.0, 0.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0)
    matrix(7).append(1.0/7.0, 0.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0, 1.0/7.0)
    //matrix(8).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0/3.0, 1.0/3.0, 1.0/3.0, 0.0)
    //matrix(9).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0/3.0, 1.0/3.0, 1.0/3.0, 0.0)
    //matrix(10).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0/3.0, 1.0/3.0, 1.0/3.0, 0.0)
    //matrix(11).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    //matrix(12).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    //matrix(13).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0)
    //matrix(14).append(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

    return matrix
  }

  def printMatrix(matrix: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]){
    for(i<-0 to matrix.length - 1){
      var res = ""

      for(j<-0 to matrix(0).length - 1){

        if(j == matrix(0).length - 1){
          res += matrix(i)(j)
        }
        else{
          res += matrix(i)(j) + "\t"
        }
      }

      println(res)
    }
    print("\n")
  }

  def printVector(vector: mutable.ArrayBuffer[Double]){
    print("(")

    for(i<-0 to vector.length - 1){

      if(i == vector.length - 1){
        print(vector(i) + ")")
      }
      else{
        print(vector(i) + ", ")
      }
    }

    print("\n")
  }

  def multiplyNumberVector(number: Double, vector: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    var res: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()

    for(i<-0 to vector.length - 1){
      res.append(number * vector(i))
    }

    return res
  }

  def multiplyVectorMatrix(vector: mutable.ArrayBuffer[Double], matrix: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]]): mutable.ArrayBuffer[Double] = {
    var res: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()

    for(i <- 0 to matrix.length - 1){
      var temp: Double = 0.0

      for(j <- 0 to matrix(0).length - 1){
        temp += vector(j) * matrix(j)(i)
      }

      res.append(temp)
    }

    return res
  }

  def multiplyVectorVector(vector1: mutable.ArrayBuffer[Double], vector2: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    var res: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()

    for(i <- 0 to vector1.length - 1){
      res.append(vector1(i)*vector2(i))
    }

    return res 
  }

  def addVector(vector1: mutable.ArrayBuffer[Double], vector2: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    var res: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()

    for(i <- 0 to vector1.length - 1){
      res.append(vector1(i) + vector2(i))
    }

    return res
  }

  def normVector(vector: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    var res: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer()

    var sum = 0.0 //getting the abs of vector
    for(i<-0 to vector.length - 1){
      sum += vector(i)
    }

    res = multiplyNumberVector(1.0/sum, vector) //multiply every value by 1/sum
    return res //return normalized vector
  }

  def printPR(vector: mutable.ArrayBuffer[Double], beta: Double, epsilon: Double){
    var pw = new FileWriter("PageRank_" + beta + "_" + epsilon + ".txt")

    for(i<-0 to 14){
      pw.write((i+1) + "\t" + vector(i) + "\n")
    }
    pw.close()
  }

  def printPR(vector: mutable.ArrayBuffer[Double], name: String){
    var pw = new FileWriter(name)

    for(i<-0 to 14){
      pw.write((i+1) + "\t" + vector(i) + "\n")
    }
    pw.close()
  }

  def getPR(beta: Double, epsilon: Double){
    var p: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer() //page rank vector
    var e: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer() //teleporting surfer
    var M = makeMatrix() //matrix M

    for(i<-0 to M.length - 1){
      p.append(1.0 / M.length) //staring p has equal values for each index
      e.append((1-beta)/M.length)
    }

    var i = 1
    while(i == 1){
      var temp = p
      p = addVector(multiplyNumberVector(beta, multiplyVectorMatrix(p, M)), multiplyVectorVector(p, e)) //calculate next page rank
      p = normVector(p) //normalize the vector

      if(abs(temp(0) - p(0)) < epsilon){
        i = 2
      }
    }

    add_cut(p)

    printPR(p, beta, epsilon)
  }

  def add_cut(p: mutable.ArrayBuffer[Double]): mutable.ArrayBuffer[Double] = {
    var temp_p = p(5)/8.0 + p(6)/8.0 + p(7)/8.0
    p.append(temp_p, temp_p, temp_p)

    temp_p = p(8)/3.0 + p(9)/3.0 + p(10)/3.0
    p.append(temp_p, temp_p, temp_p)

    temp_p = p(11) + p(12) + p(13)
    p.append(temp_p)
  }

  def mushroom(beta: Double, epsilon: Double){
    var p: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer() //page rank vector
    var M = makeMatrix() //matrix M
    var e: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer() //teleporting surfer

    for(i<-0 to M.length - 1){
      p.append(1.0 / M.length) //staring p has equal values for each index
    }

    e.append(0.0,0.5,0.5,0.0,0.0,0.0,0.0,0.0)
    e = multiplyNumberVector((1-beta), e)

    var i = 1
    while(i == 1){
      var temp = p
      p = addVector(multiplyNumberVector(beta, multiplyVectorMatrix(p, M)), multiplyVectorVector(p, e)) //calculate next page rank
      p = normVector(p) //normalize the vector

      if(abs(temp(0) - p(0)) < epsilon){
        i = 2
      }
    }

    add_cut(p)

    printPR(p, "amanitaPR.txt")
  }
}