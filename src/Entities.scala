object Entities {
  case class Pos(x: Int, y: Int) {
    def this(coord: (Int, Int)) = this(coord._1, coord._2)
    def toPair: (Int, Int) = (x, y)

    def +(p: Pos): Pos =
      new Pos(this.x + p.x, this.y + p.y)
    def -(p: Pos): Pos =
      new Pos(this.x - p.x, this.y - p.y)

    def inField: Boolean = {
      val (xmin, ymin) = (0, 0)
      val (xmax, ymax) = (99, 99)

      this.x >= xmin && this.y >= ymin &&
      this.x <= xmax && this.y <= ymax
    }
  }

  case class Answer(pos: Pos, high: Int)
}
