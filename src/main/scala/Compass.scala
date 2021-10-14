
case class Compass(WE :Int, NS :Int ){
  def +(that: Int) : Int = this.WE + 8* this.NS + that
  def -(that: Int) : Int = this.WE + 8* this.NS - that
  def +(that: Compass) :Compass = Compass(this.WE+that.WE,this.NS+that.NS)
  def -(that: Compass) :Compass = Compass(this.WE-that.WE,this.NS-that.NS)
  def toInt:Int = this.WE + 8* this.NS
  def isValidPosition :Boolean = (WE>=0 && WE<=7 && NS>=0 && NS<=7)
  override def toString: String = if (!isValidPosition) "out" else {
    "" + TextToMove.letters.charAt(WE) + TextToMove.numbers(NS)
  }
}

object Compass {
  def direction(placeFrom: Int, placeTo: Int): Compass = {
    if (placeFrom / 8 == placeTo / 8 && placeFrom < placeTo) Compass(1, 0) else
      if (placeFrom / 8 == placeTo / 8 && placeFrom > placeTo) Compass(-1, 0) else
        if(placeFrom%8 == placeTo%8  && placeFrom < placeTo) Compass(0,1) else
          if(placeFrom%8 == placeTo%8  && placeFrom > placeTo) Compass(0,-1) else
            if(CheckBoard.fieldsOnDiagonal(placeFrom,placeTo)){
      val tmp = Compass.fromInt(placeTo) - Compass.fromInt(placeFrom)
        if (tmp.WE<0 && tmp.NS<0) Compass(-1,-1) else
        if (tmp.WE>0 && tmp.NS<0) Compass( 1,-1) else
        if (tmp.WE<0 && tmp.NS>0) Compass(-1, 1) else
        if (tmp.WE>0 && tmp.NS>0) Compass( 1, 1) else
          Compass(0,0)

    } else Compass(0,0)

  }

  def fromInt(pos: Int):Compass = Compass (pos%8,pos/8)
}