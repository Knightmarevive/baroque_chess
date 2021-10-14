class doMove {

}

object doMove {
  def act(chk: CheckBoard, _from: Int, _to: Int, _side: Int, _theoretical: Boolean): Option[CheckBoard] = {
    if(chk.fields(_from).SameSide(_side ) ) {
      if (chk.fields(_to).SameSide(_side) ) {
        if(chk.isInFear(_from)){
          Some (new CheckBoard(
            for(i <- 0 to 63) yield if(i==_from) ChessPiece(0,ChessPiece.Empty) else chk.fields(i)
          ))
        } else None
      } else if (!chk.isInFear(_from)) {
        val currentPiece = chk.fields(_from).kind
        val kingPos = chk.findKing(_side)
        //println(" trying to move ("+currentPiece+")")

        if(( (! _theoretical) && currentPiece != ChessPiece.King && King.fieldIsInCheck(
          chk , kingPos, kingPos, _side )) || ((! _theoretical) && currentPiece == ChessPiece.King &&
          King.fieldIsInCheck(chk, _from, _to, _side)
          ) ) {
          // println(" This would be Ouch "); chk.printme ; println("")
          // println( " This would be ouch " + Compass.fromInt(_from) + Compass.fromInt(_to) )
          None } else if (_from==_to) None else
        currentPiece match {
          case ChessPiece.Pincer => {
            if (Pincer.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + Pincer.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          }
          case ChessPiece.King => {
            if (King.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + King.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          }
          case ChessPiece.Withdrawer => {
            if (Withdrawer.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + Withdrawer.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          }
          case ChessPiece.Dragon => {
            if (Dragon.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + Dragon.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          }
          case ChessPiece.Coordinator => {
            if (Coordinator.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + Coordinator.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          }
          case ChessPiece.LongLeaper => {
            if (LongLeaper.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + LongLeaper.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          }
          case ChessPiece.Imitator => {
            if (Imitator.fieldAvailible(chk, _from, _to, _side))
              Some(chk + (MoveEffect.moveWithoutKill(chk, _from, _to) + Imitator.fieldsToBurn(
                chk, _from, _to, _side
              )))
            else None
          } //else None

          case _ => None

        }
      } else None
    }else None

  } //else None
}
