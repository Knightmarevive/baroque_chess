object Pincer extends PieceMove {
  override def whichPiece: String = ChessPiece.Pincer
  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int) : Boolean ={
    if(!(isThere(chk, placeFrom, side)| CheckBoard.fieldsInLine(placeFrom,placeTo) ) ) return false
    val cmp = Compass.direction(placeFrom,placeTo)

    for(i <- Range(placeFrom,cmp+placeTo,cmp.toInt))
      if(chk.fields(i).side != 0)
        return false

    true
  }
}