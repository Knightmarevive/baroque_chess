trait PieceMove {
  def whichPiece: String
  def isThere(chk: CheckBoard, place: Int, side: Int) : Boolean = {
    if (place<0 || place>63) false else
    chk.fields(place) == ChessPiece(side,whichPiece)
  }
  def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int) : Boolean
  def fieldsToBurn  (chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int) : List[Int]
}
