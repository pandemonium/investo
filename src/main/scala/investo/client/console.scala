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
                        show: A => String) {
    def makeCell(row: A): Cell = 
      Cell(show(row), align)
  }

  implicit class TableTemplate[A](val columns: Seq[Column[A]]) extends AnyVal {
    def format(rows: Seq[A]): String = {
      val rs = rows.map(row => columns map(_ makeCell row))
      val cs = rs.transpose map { column =>
        val extent = column.maxBy(_.contentLength).contentLength

        column map(_ render extent)
      }

      cs.transpose map(_ mkString " | ") mkString "\n"
    }
  }
}