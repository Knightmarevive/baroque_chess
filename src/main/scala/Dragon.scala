object Dragon extends PieceMove {

  override def whichPiece: String = ChessPiece.Dragon

  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    if (CheckBoard.fieldsOnStar(placeFrom,placeTo)){

      val cmp = Compass.direction(placeFrom, placeTo)

      for (i <- Range(cmp + placeFrom, cmp + placeTo, cmp.toInt))
        if (chk.fields(i).side != 0)
          return false

      return true
    } else false
  }

  override def fieldsToBurn(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): List[Int] = {
    List[Int]()
  }
}

// TODO: Dragonify/Fear