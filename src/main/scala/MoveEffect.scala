case class MoveEffect(piece: ChessPiece, place: Int, remove: List[Int]) {
  def +(that: Int): MoveEffect = {
    MoveEffect(piece,place,remove :+ that)
  }
  def +(that: List[Int]): MoveEffect = {
    MoveEffect(piece,place,remove ++ that)
  }
}

object MoveEffect {
  def moveWithoutKill(chk: CheckBoard, moveFrom: Int, moveTo: Int) : MoveEffect ={
    MoveEffect(chk.fields(moveFrom),moveTo,List(moveFrom))
  }
}
