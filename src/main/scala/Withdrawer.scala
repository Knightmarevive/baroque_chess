object Withdrawer extends PieceMove {
  override def whichPiece: String = ChessPiece.Withdrawer
  override def ownValue: Long = 888

  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    if (CheckBoard.fieldsOnStar(placeFrom,placeTo)){

      val cmp = Compass.direction(placeFrom, placeTo)
      if(cmp == Compass(0,0)) return false


      for (i <- Range(cmp + placeFrom, cmp + placeTo, cmp.toInt))
        if (chk.fields(i).side != 0)
          return false

      return true
    } else false
  }

  override def fieldsToBurn(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): List[Int] = {
    val the_field = Compass.direction(placeTo,placeFrom) + Compass.fromInt(placeFrom) // reverse direction is intentional
    if (the_field.isValidPosition && chk.fields(the_field.toInt).OpposeSide(side))
     List[Int](the_field.toInt) else List[Int]()
  }
}
