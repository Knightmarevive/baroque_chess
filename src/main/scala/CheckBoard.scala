class CheckBoard (val fields: Vector[ChessPiece]) {
  val lineBreaker: String = "-+-+-+-+-+-+-+-+-"
  val columnMark: Vector[String] = Vector.empty[String] :+
    "8" :+ "7" :+ "6" :+ "5" :+ "4" :+ "3" :+ "2" :+ "1"
  /*
  val rowMark: Vector[String] = Vector.empty[String] :+
    "H" :+ "G" :+ "F" :+ "E" :+ "D" :+ "C" :+ "B" :+ "A"
  */
  val edgeRow   :String = " |A|B|C|D|E|F|G|H| \n"
  val middleRow :String = "-+-+-+-+-+-+-+-+-+-\n"

  def aPiece    (fieldID: Int) :String = { "|" + fields(fieldID).ChessToString}
  def twoRows   (rowID:   Int) :String = {
    middleRow +
    columnMark(rowID) +
    ( for( zCol <- 0 to 7) yield aPiece(rowID*8+zCol) ).mkString("") +
    "|" + columnMark(rowID) +"\n"}

  def printme: Unit = {
    print(edgeRow +
    ( for (zRow <- 0 to 7) yield twoRows(zRow) ).mkString("")+
    middleRow+
    edgeRow)
  }
}

object CheckBoard {
  def createEmpty:CheckBoard = new CheckBoard(Vector.fill(64){ChessPiece(0," ")})
}
