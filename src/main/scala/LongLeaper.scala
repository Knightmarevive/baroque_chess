object LongLeaper extends PieceMove {
  override def whichPiece: String = ChessPiece.LongLeaper

  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    if(chk.fields(placeTo).isEmpty){
      val cmp = Compass.direction(placeFrom,placeTo)

      for (i <- Range(cmp + placeFrom, placeTo, cmp.toInt))
        if(chk.fields(i).SameSide(side) ||
          (chk.fields(i).OpposeSide(side) && !chk.fields(cmp+i).isEmpty)
        ) return false

           true
    } else false
  }

  override def fieldsToBurn(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): List[Int] = {
    val cmp = Compass.direction(placeFrom,placeTo)

    for (i <- Range(cmp + placeFrom, placeTo, cmp.toInt).toList;
       if (chk.fields(i).OpposeSide(side)))
         yield i

  }

}
