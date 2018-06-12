class doMove {

}

object doMove {
  def act(chk: CheckBoard, _from: Int, _to: Int, _side: Int): Option[CheckBoard] = {
    if( (!chk.fields(_from).SameSide(_side ) ) || chk.fields(_to).SameSide(_side)) None else
      {
        val currentPiece = chk.fields(_from).kind
        currentPiece match {
          case ChessPiece.Pincer => {
            if (Pincer.fieldAvailible(chk,_from,_to,_side))
              Some(chk + MoveEffect.moveWithoutKill(chk,_from,_to))
            else None
          }

          case _ => None

        }
      }
  }
}
