package delta

import shapeless.labelled.FieldType
import shapeless.{::, Generic, HList, HNil, LabelledGeneric, Poly1, Typeable, Witness}
import shapeless.ops.hlist.{Mapper, Zip, ZipWithKeys}
import shapeless.ops.record.Keys
import shapeless.ops.record._

sealed trait Delta[+A]

case object Equal extends Delta[Nothing]

case class Different[A](a: A, b: A) extends Delta[A]

object comparator extends Poly1 {
  implicit def tuple[A]: Case.Aux[(A, A), Delta[A]] = at[(A, A)] { case (a1, a2) => if (a1 == a2) Equal else Different(a1, a2) }
}

/** turns a LabelledGeneric into tuples of keyname to value */
object toMap extends Poly1 {
  implicit def recordToTuple[K <: Symbol, V](implicit witness: Witness.Aux[K]): Case.Aux[FieldType[K, V], (String, V)] =
    at[FieldType[K, V]] { value => (witness.value.name, value) }
}

object Compare {
  /** compares two case class instances. returns an hlist of tagged Delta. */
  def compare[P <: Product, LG <: HList, K <: HList, G <: HList, M <: HList, Z <: HList, Out <: HList]
  (p1: P, p2: P)(implicit
                 gen: Generic.Aux[P, G],
                 lab: LabelledGeneric.Aux[P, LG],
                 zip: Zip.Aux[G :: G :: HNil, Z],
                 keys: Keys.Aux[LG, K],
                 mapper: Mapper.Aux[comparator.type, Z, M],
                 zwk: ZipWithKeys.Aux[K, M, Out]
  ): zwk.Out = {
    val gen1 = gen.to(p1)
    val gen2 = gen.to(p2)
    gen1.zip(gen2).map(comparator).zipWithKeys(keys())
  }
}
