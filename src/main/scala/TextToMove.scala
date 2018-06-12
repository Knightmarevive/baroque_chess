case class TextToMove(_from: Compass, _to: Compass) {
  def areFieldsValid: Boolean = {
    _from.NS>=0 && _from.WE>=0 &&
      _to.NS>=0 && _to.WE>=0
  }
}

object TextToMove {
  val letters: String = "ABCDEFGH"
  val numbers: String = "87654321"
  def create(str: String): TextToMove ={
    TextToMove(
      Compass( letters.indexOf(str.charAt(0)):Int,
        numbers.indexOf(str.indexOf(1)):Int ),
      Compass( letters.indexOf(str.charAt(2)):Int,
        numbers.indexOf(str.charAt(3)) :Int
      ))
  }
  def ask: TextToMove ={
    print(scala.Console.RESET + "\n enter your move ")
    val str: String = scala.io.StdIn.readLine()
    if (str.size < 4) return ask
    val ret = create(str)
    if (ret.areFieldsValid) ret else ask
  }
}
