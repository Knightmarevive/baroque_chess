import scala.Console.{GREEN,RED,BLACK,RESET}

case class ChessPiece(side:Int, kind:String){
  def printme:Unit = print( s"$RESET"+(
    if(side==1) s"$GREEN" else (if (side==2) s"$RED" else s"$BLACK")
  )+kind+s"$RESET")
}