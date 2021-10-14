import scala.Console.{GREEN_B,RED_B,BLACK,BLACK_B,RESET,BOLD,YELLOW,BLUE}
import scala.io.StdIn.readLine
// import cats.effect.IO
// import cats.Eval
// import cats.implicits._
// import scala.concurrent.Future

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
    //print("creating TextToMove")
    TextToMove(
      Compass( letters.indexOf(str.charAt(0)):Int,
        numbers.indexOf(str.charAt(1)):Int ),
      Compass( letters.indexOf(str.charAt(2)):Int,
        numbers.indexOf(str.charAt(3)) :Int
      ))
  }
  def ask: TextToMove ={
    print(scala.Console.RESET + "\n enter your move ")
    // val in: IO[String] = IO(readLine())
	//val in = IO { readLine() }
	// val in = Eval.always { readLine() }
	// val str: String = in.value
	// BufferedReader r1 = new BufferedReader(new InputStreamReader(System.in))
	val str = readLine()
	if (str == null) return ask
    if (str.size != 4) return ask
    val ret = create(str)
    if (ret.areFieldsValid) {
      println("\n trying to "+str)
      ret } else
    { println("\n " ++ str ++ " (" ++ ret.toString ++ ") is not valid move statement ")
      ask }
  }

  def sideToString(_side: Int): String = if(_side==1) s"$GREEN_B$YELLOW Green $RESET" else
    if(_side==2) s"$RED_B$BLUE Red $RESET" else " Nobody "
}
