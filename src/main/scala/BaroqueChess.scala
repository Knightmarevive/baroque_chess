object BaroqueChess {
  def main(args: Array[String]) {
    //val chk:CheckBoard = CheckBoard.createEmpty
    var chk:CheckBoard = CheckBoard.createStart
    var side: Int = 2
    while(true) {
      chk.printme
      val _move = TextToMove.ask
      val _ret = doMove.act(chk,_move._from.toInt,_move._to.toInt, side )
      if (_ret != None){
        println(" proper move ")
        if (side != 2) side =2 else side = 1
        chk=_ret.get
      }
    }
    println("")
  }
}
