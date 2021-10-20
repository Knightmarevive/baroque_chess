import scala.Console.{GREEN_B,RED_B,BLACK,BLACK_B,RESET,BOLD,YELLOW,BLUE}

case class ChessPiece(side:Int, kind:String){
  /*
  def ChessToString:String = { s"$RESET"+s"$BOLD"+(
    if(side==1) s"$GREEN_B"+s"$YELLOW" else (if (side==2) s"$RED_B" + s"$BLUE" else s"$BLACK_B"+ s"$BLACK")
  )+kind+s"$RESET"}
  */
  
  def ChessToString:String = { (
    if(side==1) s"(" else (if (side==2) s"<" else s" ")
  )+kind+(
    if(side==1) s")" else (if (side==2) s">" else s" ")
  )  }
  

 def SameSide(_side: Int):Boolean = {
    //println(" debug: SameSide "+side.toString+" "+_side.toString)
    (_side == side) }
  def OpposeSide(_side: Int):Boolean = (_side != side && _side != 0 && side != 0)
  def isEmpty: Boolean  = (side==0)
  def value(_side: Int): Long ={
    if(side==0) 0 else {
      (if(side==_side) 1L else -1L)*{
        kind match{
          case ChessPiece.King =>         King.ownValue
          case ChessPiece.Pincer =>       Pincer.ownValue
          case ChessPiece.Withdrawer =>   Withdrawer.ownValue
          case ChessPiece.LongLeaper =>   LongLeaper.ownValue
          case ChessPiece.Coordinator =>  Coordinator.ownValue
          case ChessPiece.Dragon =>       Dragon.ownValue
          case ChessPiece.Imitator =>     Imitator.ownValue

          case _ => 0
        }
      }
    }
  }
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