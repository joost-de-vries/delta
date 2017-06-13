package delta


case class Measurement(time: Long, posX: Long, posY: Long)

object SimpleExample extends App {
  val m1 = Measurement(time = 123, posX = 4, posY = 4)
  val m2 = Measurement(time = 124, posX = 4, posY = 5)

  val result = Compare.compare(m1, m2)
  val r2 = result.map(toMap).toList.toMap
  println(s"result $r2")
}

object ExampleChanges extends App {
  val events = Seq(Measurement(time = 123, posX = 4, posY = 5),
    Measurement(time = 124, posX = 4, posY = 5),
    Measurement(time = 125, posX = 5, posY = 5),
    Measurement(time = 126, posX = 5, posY = 4),
    Measurement(time = 127, posX = 6, posY = 5)
  )

  val changes = events.zip(events.tail).foldLeft(List.empty[Changes]) { case (acc, (prev, next)) => {
    val nextWithTime = next.copy(time = prev.time) //changes in timestamps are not interesting. will be filtered out
    val result = Compare.compare(prev, nextWithTime)
    val asMap = result
      .map(toMap)
      .toList
      .filter {
        case (_, Equal) => false //we're only interested in what's changed
        case _ => true
      }.toMap
    Changes(prev, next, asMap) :: acc
  }
  }.reverse

  println(changes.mkString("\n"))
}

case class Changes(prev: Measurement, next: Measurement, changes: Map[String, Delta[_]]) {
  override def toString = {
    val prefix = s"from ${prev.time} to ${next.time}:"
    if (changes.isEmpty) {
      s"$prefix \n\tno changes"
    } else {
      val msgs = changes.mapValues {
        case Equal => "not changed"
        case Different(a, b) => s"from $a to $b"
      }
      s"$prefix \n\t${msgs.mkString("\n\t")}"
    }
  }
}