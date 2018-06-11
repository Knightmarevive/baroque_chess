import scala.Console.{GREEN_B,RED_B,BLACK,BLACK_B,RESET,BOLD}

case class ChessPiece(side:Int, kind:String){
  def ChessToString:String = { s"$RESET"+s"$BLACK"+s"$BOLD"+(
    if(side==1) s"$GREEN_B" else (if (side==2) s"$RED_B" else s"$BLACK_B"+ s"$BLACK")
  )+kind+s"$RESET"}
}

object ChessPiece {
  val King        :String = "K"
  val Pincer      :String = "P"
  val Withdrawer  :String = "W"
  val LongLeaper  :String = "L"
  val Coordinator :String = "C"
  val Dragon      :String = "D"
  val Imitator    :String = "I"
  val Empty       :String = " "
}