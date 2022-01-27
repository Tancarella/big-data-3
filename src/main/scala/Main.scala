import scala.io.Source
import scala.collection._
import java.io._
import scala.math._
import Array._
import scala.util.matching.Regex

object Main extends App {
  //Crawling
  var c = new Crawler()

  //PageRank
  var p = new PageRank("Edges.txt")

  //Analysis
  var a = new Analyse("Edges.txt")

  var i = 0
  while(i == 0){
    println("To run the crawler, press 1")
    println("To analyse crawler results, press 2")
    println("To calculate page rank, press 3")
    println("To exit, press 4")
    var inp = scala.io.StdIn.readLine()

    if(inp == "1"){
      println("Input the starting page")
      var start = scala.io.StdIn.readLine()
      println("Input number of webpages you want to crawl")
      var crawl_n = scala.io.StdIn.readLine()
      try{
        c.Crawl(crawl_n.toInt, start)
      }
      catch{
        case e: NumberFormatException => println("Input not a number.")
      }
    }
    else if(inp == "2"){
      a.AvgLinks()
      println("Input the link you want to analyse")
      var link = scala.io.StdIn.readLine()
      a.AnalyseLink(link)
    }
    else if(inp == "3"){
      println("Input the value of beta")
      var beta = scala.io.StdIn.readLine()
      println("Input number of times you want page rank to be calculated")
      var pr_n = scala.io.StdIn.readLine()
      try{
        p.getPR(beta.toDouble, pr_n.toInt)
      }
      catch{
        case e: NumberFormatException => println("Input not a number.")
      }
    }
    else if(inp == "4"){
      i = 1
    }
    else{
      println("Wrong input")
    }
  }

  /*c.Crawl(100, "https://en.wikipedia.org/w/index.php?title=John_Kincaid_(political_scientist)&redirect=yes")
  p.getPR(0.9, 100)
  a.AvgLinks()
  a.AnalyseLink("https://en.wikipedia.org/w/index.php?title=George_W._Bush&redirect=yes")*/
}

class PageRank(file: String){

  def PruneNodes(): mutable.Map[String, mutable.ArrayBuffer[String]] = {
    var edges: Array[String] = Source.fromFile(file).mkString.split("\n") //get edges into array
    var crawledPages: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //list of pages we crawled
    var prunnedEdges: mutable.Map[String, mutable.ArrayBuffer[String]] = mutable.Map()

    //go through every page to get list of pages we crawled
    for(edge<-edges){
      var page = edge.split("\t").apply(0) //get the first value in string - name of page we crawled
      if(crawledPages.contains(page) == false){ //if list of pages we crawled does not contain this page, add it
        crawledPages.append(page)
      }
    }
    
    //go through every edge to make a map (page, [links])
    for(edge<-edges){
      var out = edge.split("\t").apply(0) //page we crawled
      var in = edge.split("\t").apply(1) //link we found

      if(prunnedEdges.keySet.contains(out) == false){ //if map does not contain the page yet
        prunnedEdges += (out -> mutable.ArrayBuffer()) //set the value from page to empty
      }

      if(crawledPages.contains(in) == true){ //if we crawled the page from link add it to map
        prunnedEdges(out).append(in)
      }
    }

    return prunnedEdges
  }

  def PageToFile(pages: Array[String]){
    var pw = new FileWriter("Pages.txt")

    for(page<-pages){
      pw.write(page + "\n")
    }

    pw.close()
  }

  def makeMatrix(): mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] = {
    var edges_map = PruneNodes() //get rid of links to pages we haven't crawled

    var matrix: mutable.ArrayBuffer[mutable.ArrayBuffer[Double]] = mutable.ArrayBuffer() //matrix M to calculate PageRank
    var keys = edges_map.keySet.toArray.sorted //sort the keys in the map

    PageToFile(keys)

    var i = 0
    for(out<-keys){ //rows
      matrix.append(mutable.ArrayBuffer())

      for(in<-keys){ //columns
        if(edges_map(out).contains(in) == true){ //there is an edge between A and B
          matrix(i).append(1.0/edges_map(out).length.toDouble)
        }
        else{ //no edge between A and B
          matrix(i).append(0.0)
        }
      }

      i = i + 1
    }

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

  def printPR(vector: mutable.ArrayBuffer[Double]){
    var pages = Source.fromFile("Pages.txt").mkString.split("\n")
    var map: mutable.Map[Double, String] = mutable.Map() //map holding page ranks
    var pw = new FileWriter("PageRank.txt")

    for(i<-0 to vector.length - 1){
      map += (vector(i) -> pages(i))
    }

    var sorted = map.keySet.toSeq.toArray.sorted

    for(key<-sorted){
      println("Page: " + map(key) + ", PageRank: " + key)
      pw.write(map(key) + "\t" + key + "\n")
    }

    pw.close()
  }

  def getPR(beta: Double, repetition: Int){
    var p: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer() //page rank vector
    var M = makeMatrix() //matrix M
    var e: mutable.ArrayBuffer[Double] = mutable.ArrayBuffer() //teleporting surfer

    for(i<-0 to M.length - 1){
      p.append(1.0 / M.length) //staring p has equal values for each index
      e.append((1.0 - beta) / M.length) //teleporting surfer has the same value for each index
    }

    for(i<-1 to repetition){ //calculate page rank n times
      p = addVector(multiplyNumberVector(beta, multiplyVectorMatrix(p, M)), e) //calculate next page rank
      p = normVector(p) //normalize the vector
    }

    printPR(p)
  }
}

class Crawler(){

  def ToFile(file: String, edges: mutable.Map[String, Array[String]]){
    var pw = new FileWriter(file) //save to file

    for(key<-edges.keys){ //for each page we crawled
      for(link<-edges(key)){ //for each outgoing link
        pw.write(key + "\t" + link + "\n") //write to file the edge
      }
    }

    pw.close()
  }

  def GetPage(url: String): (String, Array[String]) = {
    var page = Source.fromURL(url).mkString //get page in html
    var reg_name = "(?<=title=)[^&]*"
    var name = reg_name.r.findAllIn(url).next
    val reg_links = "(?<=<a href=\"\\/wiki\\/)(?!.*%|Category|Help|Wikipedia|Special|" +
        "Talk|Privacy_policy|Cookie_statement|Terms_of_Use|Portal|Main_Page|File|Template)[^\"]*" //regex for finding article name
    var links = reg_links.r.findAllIn(page).toSet.toArray //array containing all links
    return (name, links)
  }

  def Crawl(crawl_n: Int, start_url: String){
    var pages_list: mutable.ArrayBuffer[String] = mutable.ArrayBuffer() //empty array for pages
    var edges: mutable.Map[String, Array[String]] = mutable.Map() //empty map to keep edges

    var p = GetPage(start_url) //start crawling
    pages_list.append(p._1)
    edges += (p._1 -> p._2) //add edges going out of starting page
    for(page<-p._2){ //add each link as new starting point
      if(pages_list.contains(page) == false && pages_list.length <= crawl_n){
        pages_list.append(page)
      }
    }

    for(i<-1 to crawl_n){
      var url = "https://en.wikipedia.org/w/index.php?title=" + pages_list(i) + "&redirect=yes"
      p = GetPage(url)

      for(page<-p._2){ //add each link as new starting point
        if(pages_list.contains(page) == false && pages_list.length <= crawl_n){
          pages_list.append(page)
        }
      }

      edges += (p._1 -> p._2) //add edges going out of page
    }

    ToFile("Edges.txt", edges)
  }
}

  class Analyse(file: String){    

  def AnalyseLink(link: String){
    var links = Source.fromFile(file).mkString.split("\n") //all edges
    var reg_name = "(?<=title=)[^&]*"
    var name = reg_name.r.findAllIn(link).next
    var sum = 0
    var out: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    for(i<-links){
      var arr = i.split("\t")

      if(arr(1) == name){
        sum += 1
        out.append(arr(0))
      }
    }

    println("Total links to " + name + " = " + sum)
    println("Pages leading to " + name + ":")
    for(i<-out){
      println(i)
    }
  }

  def AvgLinks(){
    var links = Source.fromFile(file).mkString.split("\n") //all edges
    var crawled: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()
    
    for(link<-links){
      var out = link.split("\t")(0)

      if(crawled.contains(out) == false){
        crawled.append(out)
      }
    }

    println("Average number of links per page: " + links.length / crawled.length)
  }
}
