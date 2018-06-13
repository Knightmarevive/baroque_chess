object BaroqueChess {
  def main(args: Array[String]) {
    //val chk:CheckBoard = CheckBoard.createEmpty
    val sides: Int = if (args.length != 1) 2 else args(0).toInt
    if (sides < 0 || sides > 2) {
      println(" There can be only 0-2 players ")
      return}
    var chk:CheckBoard = CheckBoard.createStart
    var side: Int = 2
    if(sides==2)
    while(true) {
      chk.printme
      val _move = TextToMove.ask
      val _ret = doMove.act(chk,_move._from.toInt,_move._to.toInt, side )
      if (_ret.isDefined){
        println(" proper move ")
        if (side != 2) side =2 else side = 1
        chk=_ret.get
      }
    }
    println("")
  }
}
