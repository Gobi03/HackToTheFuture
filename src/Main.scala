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
  val ansSeq: List[Answer] = solve(0, Nil)

  def solve(cnt: Int, res: List[Answer]): List[Answer] = {
    if(cnt == 1000)
      res.reverse
    else {
      searchHighestPos(field) match {
        case (None, _) => res.reverse
        case (Some(center), high) =>
          val score = min(100, high)
          field(center.y)(center.x) -= score
          for {
            dist <- 1 to min(200, (score-1))
            pos <- mkMastangSeqByDist(center, dist)
            if pos.inField
          } {
            // println(s"pre: ${field(pos.y)(pos.x)}")
            field(pos.y)(pos.x) -= score - dist
            // println(s"post: ${field(pos.y)(pos.x)}")
          }
          solve(cnt+1, Answer(center, score) :: res)
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
