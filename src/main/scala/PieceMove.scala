trait PieceMove {
  def whichPiece: String
  def isThere(chk: CheckBoard, place: Int, side: Int) : Boolean = {
    chk.fields(place) == ChessPiece(side,whichPiece)
  }
  def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int) : Boolean
}
