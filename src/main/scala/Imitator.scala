object Imitator extends PieceMove {
    override def whichPiece: String = ChessPiece.Imitator

  override def ownValue: Long = 1111

  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    if(CheckBoard.fieldsNear(placeFrom,placeTo) &&
      chk.fields(placeTo).kind==ChessPiece.King &&
      chk.fields(placeTo).OpposeSide(side)) true else
    if(chk.fields(placeTo).isEmpty){
      val cmp = Compass.direction(placeFrom,placeTo)
      if(cmp == Compass(0,0)) return false


      for (i <- Range(cmp + placeFrom, placeTo, cmp.toInt))
        if(chk.fields(i).SameSide(side) || chk.fields(i).kind != ChessPiece.LongLeaper ||
          (chk.fields(i).OpposeSide(side) && !chk.fields(cmp+i).isEmpty)
        ) return false

      true
    } else false
  }

  override def fieldsToBurn(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): List[Int] = {
    val cmp = Compass.direction(placeFrom,placeTo)

    (for (i <- Range(cmp + placeFrom, placeTo, cmp.toInt).toList;
         if (chk.fields(i).OpposeSide(side)))
      yield i) ++ (if(CheckBoard.fieldsInLine(placeFrom,placeTo)) {
      for (ppp <- Pincer.fieldsToBurn(chk,placeFrom,placeTo,side);
           if (chk.fields(ppp).kind == ChessPiece.Pincer)
      ) yield ppp
    } else List[Int]()) ++
      (for(ccc <- Coordinator.fieldsToBurn(chk,placeFrom,placeTo,side);
    if (chk.fields(ccc).kind == ChessPiece.Coordinator)
    ) yield ccc) ++
      (for(www <- Withdrawer.fieldsToBurn(chk,placeFrom,placeTo,side);
           if (chk.fields(www).kind == ChessPiece.Withdrawer)
      ) yield www)
  //} else List[Int]())

}

}
