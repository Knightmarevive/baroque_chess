object King extends PieceMove {
  override def whichPiece: String = ChessPiece.King
  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    CheckBoard.fieldsNear(placeFrom,placeTo)
  }

  override def fieldsToBurn(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): List[Int] = {
    List[Int]()
  }
}
