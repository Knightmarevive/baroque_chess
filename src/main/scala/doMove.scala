class doMove {

}

object doMove {
  def act(chk: CheckBoard, _from: Int, _to: Int, _side: Int): Option[CheckBoard] = {
    if(chk.fields(_from).SameSide(_side ) ) {
      if (chk.fields(_to).SameSide(_side) ) {
        if(chk.isInFear(_from)){
          Some (new CheckBoard(
            for(i <- 0 to 63) yield if(i==_from) ChessPiece(0,ChessPiece.Empty) else chk.fields(i)
          ))
        } else None
      } else {
        val currentPiece = chk.fields(_from).kind
        //println(" trying to move ("+currentPiece+")")
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
          }

          case _ => None

        }
      }
    } else None

  }
}
