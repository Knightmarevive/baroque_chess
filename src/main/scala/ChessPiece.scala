import scala.Console.{GREEN_B,RED_B,BLACK,BLACK_B,RESET,BOLD}

case class ChessPiece(side:Int, kind:String){
  def printme:Unit = print( s"$RESET"+s"$BLACK"+s"$BOLD"+(
    if(side==1) s"$GREEN_B" else (if (side==2) s"$RED_B" else s"$BLACK_B")
  )+kind+s"$RESET")
}