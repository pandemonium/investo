package investo
package console

object Template {
  object Alignment {
    sealed trait T
    case object Left
      extends T
    case object Center
      extends T
    case object Right
      extends T
  }

  case class Cell(content: String, 
                alignment: Alignment.T) {
    def contentLength = content.length
    private def padding(length: Int): String =
      Seq.fill(length)(' ').mkString

    def render(extent: Int): String = alignment match {
      case Alignment.Left =>
        s"$content${padding(extent - contentLength)}"
      case Alignment.Center =>
        val padLen = extent - contentLength
        val left = padLen / 2
        val right = padLen - padLen / 2
        s"${padding(left)}$content${padding(right)}"
      case Alignment.Right =>
        s"${padding(extent - contentLength)}$content"
    }
  }

  case class Column[A](align: Alignment.T, 
                        show: A => String,
                       label: Option[String] = Option.empty) {
    def makeCell(row: A): Cell = 
      Cell(show(row), align)
  }

  implicit class TableTemplate[A](val columns: Seq[Column[A]]) extends AnyVal {
    def formatGrouped[K: Ordering]( rows: Seq[A]) 
                                  (group: A => K,
                                    show: K => String): String = {
      val labels = columns.map(c => rows.head -> makeLabelCell(c.label getOrElse ""))
      val rs = labels +: rows.map(row => columns.map(c => row -> c.makeCell(row)))
      val cs = rs.transpose map { column =>
        val extent = column.maxBy(_._2.contentLength)._2.contentLength

        column.map { case (row, c) => row -> c.render(extent) }
      }

      def makeUnderLine(cols: Seq[(A, String)]): String = 
        cols.map(_._2).map(_.map(_ => '-')) mkString "-+-"

      def makeLine(cols: Seq[(A, String)]): String = 
        cols.map(_._2) mkString " | "

      val grid      = cs.transpose
      val headLine  = makeLine(grid.head)
      val underLine = makeUnderLine(grid.head)
      val gridBody  = grid
        .tail
        .groupBy(row => group(row.head._1))
        .toSeq
        .sortBy(_._1)
        .flatMap { case (key, values) =>
          show(key) +: headLine +: underLine +: values.map(makeLine) :+ ""
        }

      gridBody mkString "\n"
    }

    def makeLabelCell(label: String) =
      Cell(label, Alignment.Center)

    def format(rows: Seq[A]): String = if (rows.nonEmpty) {
      val labels = columns.map(c => makeLabelCell(c.label getOrElse ""))
      val rs = labels +: rows.map(row => columns map(_ makeCell row))
      val cs = rs.transpose map { column =>
        val extent = column.maxBy(_.contentLength).contentLength

        column map(_ render extent)
      }

      val grid      = cs.transpose
      val headLine  = grid.head
      val body      = grid.tail

      headLine +: body mkString "\n"
    } else ""
  }
}