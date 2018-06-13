case class checkMove(_side :Int) {
    def allMoves(from :CheckBoard): List[CheckBoard] = if(_side==0) List[CheckBoard]() else (
      for(i <- (0 to 63).toList; if (from.fields(i).SameSide(_side))) yield
      if(from.isInFear(i)) List[CheckBoard](doMove.act(from,i,i,_side,true).get) else
      for(j <- (0 to 63).toList; if ( (from.fields(i).kind == ChessPiece.King && CheckBoard.fieldsNear(i,j)) ||
        (from.fields(i).kind == ChessPiece.Pincer && CheckBoard.fieldsInLine(i,j)) ||
        (from.fields(i).kind != ChessPiece.King && from.fields(i).kind != ChessPiece.Pincer &&
          CheckBoard.fieldsOnStar(i,j))) && (doMove.act(from,i,j,_side, true).isDefined )) yield doMove.act(from,i,j,_side, true).get
      ).flatten

    def switch: checkMove = if(_side==1) checkMove(2) else if (_side==2) checkMove(1) else checkMove(0)
}

object checkMove {
  val upper = checkMove(1)
  val lower = checkMove(2)
  def Opponent(player :Int) :Int = if (player==1) 2 else  if (player==2) 1 else 0
}
