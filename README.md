# delta
If you have an eventlog of case class instances and you want show the end user a message what's changed without a lot of boilerplate. This uses Shapeless.

scala
```
case class Measurement(time: Long, posX: Long, posY: Long)

object SimpleExample extends App {
  val m1 = Measurement(time = 123, posX = 4, posY = 4)
  val m2 = Measurement(time = 124, posX = 4, posY = 5)

  val result = Compare.compare(m1, m2)
  val r2 = result.map(toMap).toList.toMap  //Map(time -> Different(123,124), posX -> Equal, posY -> Different(4,5))
```
