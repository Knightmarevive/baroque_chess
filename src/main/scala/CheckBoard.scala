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
  def createStart:CheckBoard = new CheckBoard( (Vector.empty[ChessPiece] :+
    ChessPiece(1,"C") :+ ChessPiece(1,"L") :+ ChessPiece(1,"I") :+ ChessPiece(1,"W") :+
    ChessPiece(1,"K") :+ ChessPiece(1,"I") :+ ChessPiece(1,"L") :+ ChessPiece(1,"D")) ++
    ( Vector.fill(8){ChessPiece(1,"P")} ++ Vector.fill(32){ChessPiece(0," ")} ++
    Vector.fill(8){ChessPiece(2,"P")} ) ++ ( Vector.empty[ChessPiece] :+
    ChessPiece(2,"D") :+ ChessPiece(2,"L") :+ ChessPiece(2,"I") :+ ChessPiece(2,"W") :+
    ChessPiece(2,"K") :+ ChessPiece(2,"I") :+ ChessPiece(2,"L") :+ ChessPiece(2,"C") ) )

}
