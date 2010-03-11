package ca

import scala.Math.{E, exp, log}

trait Utilities {
  def log2(x: Double): Double = log(x)/log(2)
  // e.g. or
  // val log2 = log(_: Double)/log(2)
  // or
  // val logx = log(_: Double)/log(_: Int)
  // val log2 = logx(_: Double, 2)
  // or simple functions
  // val log2 = (x: Double) => log(x)/log(2)
  // or
  // var logx = (x: Double) => log(x)/log(2) for base 2
  // logx = (x: Double) => log(x)/log(3) change to base 3
  // def logAny(base: Double)(x: Double): Double = log(x)/log(base)
  
  def exp2(x: Double): Int = (exp(x)/exp(x*log(E/2))).toInt
  
  def toBinary(x: Int): Array[Int] = {
    def maximum(y: Int, i: Int): Int = {
      if (y >= exp2(i+1))
        maximum(y, i+1)
      else
        i+1
    }
    val maxval = maximum(x, 0)
    val input = new Array[Int](maxval)
    val max = (0 to (maxval - 1)).reverse.toArray
    
    def decomposer(y: Int, output: Array[Int], maximum: Array[Int], i: Int): Array[Int] = {
      if (x == 0)
        output
      else if (i > maximum.length)
        output
      else if (y - exp2(maximum(i)).toInt == 0) {
        output(i) = 1
        output
      }
      else if (log2(y) >= maximum(i)) {
        output(i) = 1
        decomposer(y-exp2(maxval-1-(i)).toInt, output, maximum, i+1)
      }
      else
        decomposer(y, output, maximum, i+1)
    }
    val output = decomposer(x, input, max, 0)
    output
    }
  
  def fromBinary(input: Array[Int]): Int = {
    var output = 0
    val max = input.length - 1
    for (i <- (0 to max)) {
      if (input(i) != 0)
        output += exp2(max-i) }
    output
  }
  
  def arrayLengthen(length: Int, second: Array[Int]): Array[Int] = {
    if (length > second.length) {
      val difference = length - second.length
      val newsecond = new Array[Int](length)
      for (i <- (0 until difference)) {
        newsecond(i) = 0
      }
      for (i <- (0 until second.length)) {
        newsecond(difference + i) = second(i)
      }
    newsecond
  }
    else
      second
  }
  
  def arrayToString(input: Array[Int]): String = {
      var output = ""
      for (i <- (0 until input.length))
        output += input(i)
      output
  }
}