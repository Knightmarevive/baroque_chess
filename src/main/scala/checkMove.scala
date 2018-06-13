case class checkMove(_side :Int) {
    def allMoves(from :CheckBoard): List[CheckBoard] = {
      (for(i <- (0 to 63).toList; if (from.fields(i).SameSide(_side))) yield
        if(from.isInFear(i)) List[CheckBoard](doMove.act(from,i,i,_side).get) else
        for(j <- (0 to 63).toList; if(((from.fields(i).kind == ChessPiece.King && CheckBoard.fieldsNear(i,j)) ||
          (from.fields(i).kind == ChessPiece.Pincer && CheckBoard.fieldsInLine(i,j)) ||
          (from.fields(i).kind != ChessPiece.King && from.fields(i).kind != ChessPiece.Pincer &&
            CheckBoard.fieldsOnStar(i,j))) && (doMove.act(from,i,j,_side).isDefined ) )) yield doMove.act(from,i,j,_side).get
        ).flatten
    }
}
