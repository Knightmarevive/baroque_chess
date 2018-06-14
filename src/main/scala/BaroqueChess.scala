

object BaroqueChess {
  def main(args: Array[String]) {
    //val chk:CheckBoard = CheckBoard.createEmpty
    val sides: Int = if (args.length != 1 && args.length !=2) 2 else args(0).toInt
    val depth: Int = if (args.length != 2) 4 else args(1).toInt

    if (sides < 0 || sides > 2) {
      println(" There can be only 0-2 players ")
      return}

    if (depth < 1 || depth > 255) {
      println(" There can be only 1-255 depth ")
      return}


    var chk:CheckBoard = CheckBoard.createStart
    var side: Int = 2
    var resolved: Boolean = false

    if(sides==2)
    while(!resolved) {
      chk.printme
      if(checkMove(side).lost(chk)){
        println (TextToMove.sideToString(side)+" Lost")
      } else println (TextToMove.sideToString(side)+" have turn.")
      val _move = TextToMove.ask
      val _ret = doMove.act(chk,_move._from.toInt,_move._to.toInt, side, false )
      if (_ret.isDefined){
        println(" proper move ")
        if (side != 2) side =2 else side = 1
        chk=_ret.get
      }
    } else if (sides==1)
      while (!resolved){
        chk.printme
        if(checkMove(side).lost(chk)){
          println (TextToMove.sideToString(side)+" Lost")
        } else println (TextToMove.sideToString(side)+" have turn.")
        if (side == 2)  {
          val _move = TextToMove.ask
          val _ret = doMove.act(chk,_move._from.toInt,_move._to.toInt, side, false )
          if (_ret.isDefined) {
            println(" proper move ")
            side = 1
            chk = _ret.get
          }
          } else {
            chk = checkMove(1).ComputerMove(chk,depth)
            side = 2
          }
      } else
      while (!resolved){
        chk.printme
        if(checkMove(side).lost(chk)){
          println (TextToMove.sideToString(side)+" Lost")
        } else println (TextToMove.sideToString(side)+" have turn.")
        if (side == 2){
          chk = checkMove(2).ComputerMove(chk,depth)
          side= 1
        } else {
          chk = checkMove(1).ComputerMove(chk,depth )
          side = 2
        }
      }

    println("")
  }
}
