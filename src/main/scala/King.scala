object King extends PieceMove {
  override def whichPiece: String = ChessPiece.King

  override def ownValue: Long = 9999999

  def fieldIsInCheck(chk: CheckBoard,placeFrom: Int, placeTo: Int,_side:Int): Boolean ={
    val chkOpp = chk + MoveEffect.moveWithoutKill(chk,placeFrom,placeTo)
    val tryOpp = checkMove(checkMove.Opponent(_side))
    for (i <- tryOpp.allMoves(chkOpp)) if(i.findKing(_side)<0) {

      return true
    }
    false
  }
  override def fieldAvailible(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): Boolean = {
    CheckBoard.fieldsNear(placeFrom,placeTo) && (!fieldIsInCheck(chk,placeFrom: Int, placeTo: Int, side))
  }

  override def fieldsToBurn(chk: CheckBoard, placeFrom: Int, placeTo: Int, side: Int): List[Int] = {
    List[Int]()
  }
}
