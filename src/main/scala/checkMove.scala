case class ValuedCheckboard(_value: Long, chk: CheckBoard) extends Ordered[ValuedCheckboard] {
  def compare(that :ValuedCheckboard): Int = this._value compare that._value
}

case class checkMove(_side :Int) {
    def allMoves(from :CheckBoard): List[CheckBoard] = if(_side==0) List[CheckBoard]() else (
      for(i <- (0 to 63).toList; if (from.fields(i).SameSide(_side))) yield
      if(from.isInFear(i)) List[CheckBoard](doMove.act(from,i,i,_side,true).get) else
      for(j <- (0 to 63).toList; if ( (from.fields(i).kind == ChessPiece.King && CheckBoard.fieldsNear(i,j)) ||
        (from.fields(i).kind == ChessPiece.Pincer && CheckBoard.fieldsInLine(i,j)) ||
        (from.fields(i).kind != ChessPiece.King && from.fields(i).kind != ChessPiece.Pincer &&
          CheckBoard.fieldsOnStar(i,j))) && (doMove.act(from,i,j,_side, true).isDefined )) yield doMove.act(from,i,j,_side, true).get
      ).flatten

    def DethroneMoves (from :CheckBoard): List[CheckBoard] = for (move <- allMoves(from) ;
                                          if(move.findKing(checkMove.Opponent(_side))<0) ) yield move

    def switch: checkMove = if(_side==1) checkMove(2) else if (_side==2) checkMove(1) else checkMove(0)

    def lost(from :CheckBoard) :Boolean = (for (move <- (allMoves(from).toParArray)) yield
      (if (switch.DethroneMoves(move).size>0) 0 else 1)).sum == 0

    def NegaScout(from :CheckBoard, alpha: Long, beta: Long, depth: Int): Long = {
      if(depth==0) from.punctation(_side) else {
          var a=alpha; var b=beta; var i=1;
          for(pos <- allMoves(from)){

            val t = -checkMove(checkMove.Opponent(_side)).NegaScout(pos,-b,-a,depth-1)
            if( (t>a) && (t<beta) && (i>1) && (depth>1))
              a= -checkMove(checkMove.Opponent(_side)).NegaScout(pos,-beta,-t,depth-1)
            a=List[Long](a,t).max
            if(a>=beta)
              return a

            b=a+1
            i+=1
          }
          return a
      }
      //todo make it more functional
    }

    def ComputerMove(from :CheckBoard, depth :Int) : CheckBoard = {
      (for (chk <- allMoves(from).toParArray) yield
        ValuedCheckboard( NegaScout(chk,(-9L)*King.ownValue,9L*King.ownValue,depth),chk ) ).max.chk
    }
}

object checkMove {
  val upper = checkMove(1)
  val lower = checkMove(2)
  def Opponent(player :Int) :Int = if (player==1) 2 else  if (player==2) 1 else 0
}
