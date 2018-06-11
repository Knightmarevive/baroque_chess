class CheckBoard (val fields: Vector[ChessPiece]) {
  val lineBreaker: String = "-+-+-+-+-+-+-+-+-"
  val columnMark: Vector[String] = Vector.empty[String] :+
    " " :+ "1" :+ "2" :+ "3" :+ "4" :+ "5" :+ "6" :+ "7" :+ "8" :+ " "
  val rowMark: Vector[String] = Vector.empty[String] :+
    " " :+ "H" :+ "G" :+ "F" :+ "E" :+ "D" :+ "C" :+ "B" :+ "A" :+ " "

}
