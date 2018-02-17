object CommonPackage {
  import Entities._

  // @center からの mastang距離 @distance の座標一覧を返す
  def mkMastangSeqByDist(center: Pos, distance: Int): Seq[Pos] = {
    def mkPosSeq(x: Int, y: Int): Seq[Pos] = {
      val vols = (x, y) match {
        case (0, _) => Seq(Pos(x, y), Pos(x, -y))
        case (_, 0) => Seq(Pos(x, y), Pos(-x, y))
        case _ => Seq(Pos(x, y), Pos(x, -y), Pos(-x, y), Pos(-x, -y))
      }
      vols.map(_ + center)
    }
    
    for {
      x <- (0 to distance).toSeq
      y = distance - x
      pos <- mkPosSeq(x, y)
    } yield pos
  }

  def searchHighestPos(field: Array[Array[Int]]): (Option[Pos], Int) = {
    var res: Option[Pos] = None
    var maxH = 0

    for(y <- 0 until 100; x <- 0 until 100) {
      val now = field(y)(x)
      if(now > maxH) {
        res = Some(Pos(x, y))
        maxH = now
      }
    }

    (res, maxH)
  }
}

