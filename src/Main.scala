import Entities._
import CommonPackage._

import java.io._
import java.util.StringTokenizer
import scala.util.control.Breaks.{breakable,break}
import Math._
import MainFuncs._


object Main extends App {
  val N = 100
  val field = Array.ofDim[Int](N, N)


  /* input */
  val in = new InputReader(System.in)  
  for(y <- 0 until N; x <- 0 until N) {
    field(y)(x) = in.next().toInt
  }
  

  /* main process */
  val ansSeq: List[Answer] = solve(0, Nil, false)

  def solve(cnt: Int, res: List[Answer], flag: Boolean): List[Answer] = {
    if(cnt == 1000)
      res.reverse
    else {
      searchHighestPos(field) match {
        case (None, _) => res.reverse
        case (Some(center), high) =>
          val score =
            if(!flag) min(100, high)
            else 1

          field(center.y)(center.x) -= score
          for {
            dist <- 1 to min(200, (score-1))
            pos <- mkManhattanSeqByDist(center, dist)
            if pos.inField
          } {
            field(pos.y)(pos.x) -= score - dist
          }

          if(!flag && sumFieldScore(field) < 0)
            solve(cnt, res, true)
          else
            solve(cnt+1, Answer(center, score) :: res, flag)
      }
    }
  }


  /* output */
  printer(ansSeq)

}

object MainFuncs {
  def printer(ansSeq: Seq[Answer]): Unit = {
    val pw = new PrintWriter(System.out)
    pw.println(ansSeq.length)
    ansSeq.foreach { ans =>
      val pos = ans.pos
      pw.println(s"${pos.x} ${pos.y} ${ans.high}")
    }

    pw.flush()
    pw.close()
  }
}

class InputReader(stream: InputStream) {
  private val reader = new BufferedReader(new InputStreamReader(stream))
  private var tokenizer: StringTokenizer = new StringTokenizer(reader.readLine())
  def next(): String = {
    while (!tokenizer.hasMoreTokens()) {
      tokenizer = new StringTokenizer(reader.readLine())
    }
    tokenizer.nextToken()
  }
}
