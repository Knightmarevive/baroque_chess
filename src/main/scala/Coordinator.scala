object Coordinator extends PieceMove {
  override def whichPiece: String = ChessPiece.Coordinator

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
    val KingPos = Compass.fromInt(chk.findKing(side))
    val CoordinatorPos = Compass.fromInt(placeTo)
    List[Int]() ++ {
      val zfield = Compass(KingPos.WE,CoordinatorPos.NS).toInt
      if(chk.fields(zfield).OpposeSide(side)) List[Int](zfield) else List[Int]()
    } ++ {
      val zfield = Compass(CoordinatorPos.WE,KingPos.NS).toInt
      if(chk.fields(zfield).OpposeSide(side)) List[Int](zfield) else List[Int]()
    }
  }
}