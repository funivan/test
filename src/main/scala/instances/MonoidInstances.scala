package instances

import definitions.{Monoid, Operation}
import parser.operations._

trait MonoidInstances {
  implicit val mOperation: Monoid[Operation] = new Monoid[Operation] {
    override def empty: Operation = Empty
    override def combine(x: Operation, y: Operation): Operation = Next(x, y)
  }
}
