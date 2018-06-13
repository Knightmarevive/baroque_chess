class CheckBoard (val fields: scala.collection.immutable.IndexedSeq[ChessPiece]) {
  val lineBreaker: String = "-+-+-+-+-+-+-+-+-"
  val columnMark: Vector[String] = Vector.empty[String] :+
    "8" :+ "7" :+ "6" :+ "5" :+ "4" :+ "3" :+ "2" :+ "1"
  /*
  val rowMark: Vector[String] = Vector.empty[String] :+
    "H" :+ "G" :+ "F" :+ "E" :+ "D" :+ "C" :+ "B" :+ "A"
  */
  val edgeRow   :String = " |A|B|C|D|E|F|G|H| \n"
  val middleRow :String = "-+-+-+-+-+-+-+-+-+-\n"

  def aPiece    (fieldID: Int) :String = { "|" + fields(fieldID).ChessToString}
  def twoRows   (rowID:   Int) :String = {
    middleRow +
    columnMark(rowID) +
    ( for( zCol <- 0 to 7) yield aPiece(rowID*8+zCol) ).mkString("") +
    "|" + columnMark(rowID) +"\n"}

  def printme: Unit = {
    print( "\n" + edgeRow +
    ( for (zRow <- 0 to 7) yield twoRows(zRow) ).mkString("")+
    middleRow+
    edgeRow)
  }

  def +(effect: MoveEffect) :CheckBoard = {
    new CheckBoard( for( i <- 0 to 63) yield
      if (effect.place == i) effect.piece else
      if(effect.remove.contains(i)) ChessPiece(0,ChessPiece.Empty) else
        fields(i)
    )
  }

  def findKing(side :Int) :Int = {
    if(side!=1 && side!=2) println(" King have to have a side ")
    for (i <- 0 to 63){
      if (fields(i).kind == ChessPiece.King &&  fields(i).SameSide(side)) return i
    }
    return -1
  }

  def isInFear(field :Int) :Boolean = {
    val side = fields(field).side
    val dirs = List(Compass(-1,-1),Compass( 0,-1),Compass( 1,-1),
                    Compass(-1, 0)               ,Compass( 1, 0),
                    Compass(-1, 1),Compass( 0, 1),Compass( 1, 1))
    for(dir <- dirs) {
      if((dir + Compass.fromInt(field)).isValidPosition ) {
        if ((fields(dir + field)).OpposeSide(side)){
          if((fields(dir + field)).kind== ChessPiece.Dragon ||
            ((fields(dir + field)).kind== ChessPiece.Imitator &&
              fields(field).kind == ChessPiece.Dragon)) return true
        }
      }
    }
    false
  }

  def punctation(side: Int):Long ={
    (for (i <- 0 to 63) yield fields(i).value(side)).sum
  }
}

object CheckBoard {
  def createEmpty:CheckBoard = new CheckBoard(Vector.fill(64){ChessPiece(0," ")})
  def createStart:CheckBoard = new CheckBoard( (Vector.empty[ChessPiece] :+
    ChessPiece(1,"C") :+ ChessPiece(1,"L") :+ ChessPiece(1,"I") :+ ChessPiece(1,"W") :+
    ChessPiece(1,"K") :+ ChessPiece(1,"I") :+ ChessPiece(1,"L") :+ ChessPiece(1,"D")) ++
    ( Vector.fill(8){ChessPiece(1,"P")} ++ Vector.fill(32){ChessPiece(0," ")} ++
    Vector.fill(8){ChessPiece(2,"P")} ) ++ ( Vector.empty[ChessPiece] :+
    ChessPiece(2,"D") :+ ChessPiece(2,"L") :+ ChessPiece(2,"I") :+ ChessPiece(2,"W") :+
    ChessPiece(2,"K") :+ ChessPiece(2,"I") :+ ChessPiece(2,"L") :+ ChessPiece(2,"C") ) )

  def fieldsInLine(begin :Int, end :Int) :Boolean = {
    begin%8==end%8 || begin/8==end/8
  }

  def fieldsOnDiagonal(begin :Int, end :Int) :Boolean = {
    val shift = (Compass.fromInt(end) - Compass.fromInt(begin))
    scala.math.abs(shift.WE) == scala.math.abs(shift.NS)
  }

  def fieldsOnStar(begin :Int, end :Int) :Boolean = {
    fieldsInLine(begin,end) || fieldsOnDiagonal(begin,end)
  }

  def fieldsNear(begin :Int, end :Int) :Boolean = {
    val shift = (Compass.fromInt(end) - Compass.fromInt(begin))
    shift != Compass(0,0) && shift.NS>= -1 && shift.NS<= 1 && shift.WE>= -1 && shift.WE<=1
  }
}
