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

  val zero = List.empty[Changes]

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

  println(s"results ${changes.mkString("\n")}")
}

case class Changes(prev:Measurement,next:Measurement, changes:Map[String,Delta[_]]){
  override def toString={
    val prefix = s"from ${prev.time} to ${next.time}:"
    if(changes.isEmpty){
      s"$prefix no changes"
    }else{
      val msgs = changes.mapValues{
        case Equal => "not changed"
        case Different(a,b) => s"from $a to $b"
      }
      s"$prefix \n\t${msgs.mkString("\n\t")}"
    }
  }
}