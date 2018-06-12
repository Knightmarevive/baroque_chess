object Pincer extends PieceMove {
  override def whichPiece: String = ChessPiece.Pincer

  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    //println(" trying to move Pincer")
    if ((placeTo < 0 || placeTo > 63) ||
      !(isThere(chk, placeFrom, side) || CheckBoard.fieldsInLine(placeFrom, placeTo))
    ) return false
    val cmp = Compass.direction(placeFrom, placeTo)

    for (i <- Range(cmp + placeFrom, cmp + placeTo, cmp.toInt))
      if (chk.fields(i).side != 0)
        return false

    true
  }

  override def fieldsToBurn(chk: CheckBoard, placeTo: Int, side: Int): List[Int] = {
    List[Int]() ++ {
      if ((Compass.fromInt(placeTo) + Compass(-2, 0)).isValidPosition &&
        chk.fields((Compass.fromInt(placeTo) + Compass(-2, 0)).toInt).SameSide(side) &&
        chk.fields((Compass.fromInt(placeTo) + Compass(-1, 0)).toInt).OpposeSide(side)) {
        List[Int]((Compass.fromInt(placeTo) + Compass(-1, 0)).toInt)
      } else List[Int]()
    } ++ {
      if ((Compass.fromInt(placeTo) + Compass(2, 0)).isValidPosition &&
        chk.fields((Compass.fromInt(placeTo) + Compass(2, 0)).toInt).SameSide(side) &&
        chk.fields((Compass.fromInt(placeTo) + Compass(1, 0)).toInt).OpposeSide(side)) {
        List[Int]((Compass.fromInt(placeTo) + Compass(1, 0)).toInt)
      } else List[Int]()

    }++ {
      if ((Compass.fromInt(placeTo) + Compass(0, -2)).isValidPosition &&
        chk.fields((Compass.fromInt(placeTo) + Compass(0, -2)).toInt).SameSide(side) &&
        chk.fields((Compass.fromInt(placeTo) + Compass(0, -1)).toInt).OpposeSide(side)) {
        List[Int]((Compass.fromInt(placeTo) + Compass(0, -1)).toInt)
      } else List[Int]()

    }++ {
      if ((Compass.fromInt(placeTo) + Compass(0, 2)).isValidPosition &&
        chk.fields((Compass.fromInt(placeTo) + Compass(0, 2)).toInt).SameSide(side) &&
        chk.fields((Compass.fromInt(placeTo) + Compass(0, 1)).toInt).OpposeSide(side)) {
        List[Int]((Compass.fromInt(placeTo) + Compass(0, 1)).toInt)
      } else List[Int]()

    }
  }
}