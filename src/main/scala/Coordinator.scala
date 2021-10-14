object Coordinator extends PieceMove {
  override def whichPiece: String = ChessPiece.Coordinator
  override def ownValue: Long = 9999

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
    val _King_Pos_ = chk.findKing(side)
    val KingPos = Compass.fromInt(_King_Pos_)
    val CoordinatorPos = Compass.fromInt(placeTo)
    if (_King_Pos_ < 0) List[Int]() else
    List[Int]() ++ {
      val zfield = Compass(KingPos.WE,CoordinatorPos.NS).toInt
      if(chk.fields(zfield).OpposeSide(side)) List[Int](zfield) else List[Int]()
    } ++ {
      val zfield = Compass(CoordinatorPos.WE,KingPos.NS).toInt
      if(chk.fields(zfield).OpposeSide(side)) List[Int](zfield) else List[Int]()
    }
  }
}
