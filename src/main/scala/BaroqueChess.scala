

object BaroqueChess {
  def main(args: Array[String]) {
    //val chk:CheckBoard = CheckBoard.createEmpty
    val sides:    Int = if (args.length < 1) 0     else args(0).toInt
    var depth:    Int = if (args.length < 2) 3     else args(1).toInt
    val every:    Int = if (args.length < 3) 144   else args(2).toInt
    val maxdepth: Int = if (args.length < 4) 7     else args(3).toInt

    if (sides < 0 || sides > 2) {
      println(" There can be only 0-2 players ")
      return}

    if (depth < 1 || depth > 255) {
      println(" There can be only 1-255 depth ")
      return}

    if (every < 10 || every > 65535){
      println(" There can be only 10-65535 progression ")
      return
    }

    if (maxdepth < depth || maxdepth > 255) {
      println(" There can be only depth-255 maxdepth ")
      return}


    var chk:CheckBoard = CheckBoard.createStart
    var side: Int  = 2
    var turn: Long = 1
    var resolved: Boolean = false

    //println (TextToMove.sideToString(side)+" have turn.")

    if(sides==2)
    while(!resolved) {
      chk.printme
      print ("\n"+TextToMove.sideToString(side)+"\t have turn.")
      if(turn%every==0 && depth<maxdepth) depth+=1

      val _move = TextToMove.ask
      val _ret = doMove.act(chk,_move._from.toInt,_move._to.toInt, side, false )
      if (_ret.isDefined){
        println(" proper move ")
        if (side != 2) side =2 else side = 1
        chk=_ret.get
      }

      turn+=1

      if(checkMove(side).lost(chk)){
        chk.printme
        resolved=true
        println (TextToMove.sideToString(side)+"\t Lost")
      } // else println (TextToMove.sideToString(side)+" have turn.")

    } else if (sides==1)
      while (!resolved){
        chk.printme
        print ("\n"+TextToMove.sideToString(side)+"\t have turn.")
        if(turn%every==0 && depth<maxdepth) depth+=1

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

        turn+=1
        if(checkMove(side).lost(chk)){
          chk.printme
          resolved=true
          println (TextToMove.sideToString(side)+"\t Lost")
        } // else println (TextToMove.sideToString(side)+" have turn.")

      } else
      while (!resolved){
        chk.printme
        print ("\n"+TextToMove.sideToString(side)+"\t have turn.")
        if(turn%every==0 && depth<maxdepth) depth+=1

        if (side == 2){
          chk = checkMove(2).ComputerMove(chk,depth)
          side= 1
        } else {
          chk = checkMove(1).ComputerMove(chk,depth )
          side = 2
        }

        turn+=1
        if(checkMove(side).lost(chk)){
          chk.printme
          resolved=true
          println (TextToMove.sideToString(side)+"\t Lost")
        } // else println (TextToMove.sideToString(side)+" have turn.")
      }

    println("")
  }
}
