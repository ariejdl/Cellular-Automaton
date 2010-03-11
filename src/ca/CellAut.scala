package ca

class CellAut ( // index values go from 0 to 255 for example
  val r: Int,
  val k: Int // number of possible values of cell - assume can only be 2
  ) extends Utilities {
  val det = (2*r)+1 // number of determinants - usually 3
  val dets = exp2(det) // number of matchable arrays - usually 8
  
  def generator(input: Array[Int], index: Int, steps: Int): Unit = {
    
    def arraySplitter(lattice: Array[Int], latticeindex: Int): Array[Int] = { // splits lattice for matching
      val output = new Array[Int](det)
      for (i <- (0 until det)) {
        output(i) = lattice((lattice.length + latticeindex + i - r) % lattice.length)
        }
      output
    }
    
    def indexer(against: Array[Int]): Int = {
      // takes an array to be matched against from arraySplitter and returns the new derived value
      val x = fromBinary(against) // for r = 1, k = 2: 0 - 7
      val formattedarray = arrayLengthen(toBinary(exp2(dets) - 1).length, toBinary(index))
      val output = formattedarray.reverse(x)
      output
    }
    
    def matcher(lattice: Array[Int], latticeindex: Int): Int = {
      // Takes each index of the array to be matched, starting at 0
      val tobematched = arraySplitter(lattice, latticeindex)
      val result = indexer(tobematched)
      result
    }
    
    def iterateOnce(previousiteration: Array[Int]): Array[Int] = {
      // produces a string using arraySplitter and matcher
      val nextiteration = new Array[Int](previousiteration.length)
      for (i <- (0 until previousiteration.length))
        nextiteration(i) = matcher(previousiteration, i)
      nextiteration
    }
    
    def iterator(iteration: Array[Int], output: Array[String]): Array[String] = {
      var cached = iteration
      output(0) = arrayToString(cached)
      for (i <- (1 to steps - 1)) {
        cached = iterateOnce(cached)
        output(i) = arrayToString(cached)
      }
      output
    }
    
    val output = new Array[String](steps)
    val finish = iterator(input, output)
    val stringitised = finish.mkString("\n")
    println(stringitised)
  }
}

object Principal {
  def main(args: Array[String]) {
    val CA1 = new CellAut(1, 2)
    val input = new Array[Int](41)
    input(20) = 1
    CA1.generator(input, 90, 50) // initial array, index value and number of iterations
    // multiples of 15 around the half-way mark (255/2) e.g. 105, 120, 135 are generally interesting
  }
}
