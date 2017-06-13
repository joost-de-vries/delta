package delta


case class Measurement(time: Long, x: Long, y: Long)

object SimpleExample extends App {
  val m1=Measurement(time = 123, x = 4, y = 4)
  val m2=Measurement(time = 124, x = 4, y = 5)

      val result = Compare.compare(m1, m2)
      val r2 = result.map(toMap).toList.toMap
    println(s"result $r2")
}

  object ExampleChanges extends App{
  val events = Seq(Measurement(time = 123, x = 4, y = 5),
    Measurement(time = 124, x = 4, y = 5),
    Measurement(time = 125, x = 5, y = 5)
  )

  val zero = List.empty[Changes[Measurement]]

  val changes = events.zip(events.tail).foldLeft(zero) { case (acc, (prev, next)) => {
    val result = Compare.compare(prev, next)
    val asMap = result
      .map(toMap).
      toList
      .filter {
        case (k, _: Equal.type) =>
          false //we're only interested in what's changed
        case ("time", _) => false //changes in timestamps are not interesting
        case _ => true
      }.toMap
    Changes(prev,next,asMap) :: acc
  }
  }.reverse

  println(s"results $changes")
}

case class Changes[A](prev:A,next:A, changes:Map[String,Delta[_]])