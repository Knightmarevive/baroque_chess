case class TextToMove(_from: Compass, _to: Compass) {

}

object TextToMove {
  val letters: String = "HGFEDCBA"
  val numbers: String = "87654321"
  def create(str: String): TextToMove ={
    TextToMove(
      Compass( letters.find(
          str.charAt(0) == _ ),
        numbers.find(
          str.charAt(1)== _)),
      Compass( letters.find(
          str.charAt(2) == _),
        numbers.find(
          str.charAt(3)== _))
    )
  }
}
