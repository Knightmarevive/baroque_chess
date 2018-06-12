case class TextToMove(_from: Compass, _to: Compass) {

}

object TextToMove {
  val letters: String = "HGFEDCBA"
  val numbers: String = "87654321"
  def create(str: String): TextToMove ={
    TextToMove(
      Compass( letters.indexOf(str.charAt(0)):Int,
        numbers.indexOf(str.indexOf(1)):Int ),
      Compass( letters.indexOf(str.charAt(2)):Int,
        numbers.indexOf(str.charAt(3)) :Int
      ))
  }
}
