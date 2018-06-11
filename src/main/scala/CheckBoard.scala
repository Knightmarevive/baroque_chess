class CheckBoard (val fields: Vector[ChessPiece]) {
  val lineBreaker: String = "-+-+-+-+-+-+-+-+-"
  val columnMark: Vector[String] = Vector.empty[String] :+
    "8" :+ "7" :+ "6" :+ "5" :+ "4" :+ "3" :+ "2" :+ "1"
  /*
  val rowMark: Vector[String] = Vector.empty[String] :+
    "H" :+ "G" :+ "F" :+ "E" :+ "D" :+ "C" :+ "B" :+ "A"
  */
  val edgeRow   :String = " |A|B|C|D|E|F|G|H| "
  val middleRow :String = "-+-+-+-+-+-+-+-+-+-"

  def aPiece (fieldID: Int) :String = { "|" + fields(fieldID).ChessToString}


  def printme: Unit = {
    println(edgeRow)

    println(middleRow)
    println(edgeRow)
  }
}

object CheckBoard {

}
