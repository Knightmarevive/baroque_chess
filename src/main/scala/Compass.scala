
case class Compass(WE :Int, NS :Int ){
  def +(that: Int) : Int = that + this.WE + 8* this.NS
  def toInt:Int = this.WE + 8* this.NS
}

object Compass {
  def direction(placeFrom: Int, placeTo: Int): Compass = {
    if (placeFrom / 8 == placeTo / 8 && placeFrom < placeTo) Compass(1, 0) else
      if (placeFrom / 8 == placeTo / 8 && placeFrom > placeTo) Compass(-1, 0) else
        if(placeFrom%8 == placeTo%8  && placeFrom < placeTo) Compass(0,1) else
          if(placeFrom%8 == placeTo%8  && placeFrom > placeTo) Compass(0,-1) else


    Compass(0,0)

  }

  def fromInt(pos: Int):Compass = Compass (pos%8,pos/8)
}