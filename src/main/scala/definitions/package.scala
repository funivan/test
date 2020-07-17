import scala.collection.immutable.HashMap
package object definitions {
  type UserInstructions = HashMap[String, Operation]

  implicit final class SemigroupSyntax[A](val x: A) extends AnyVal {
    def |+|(y: A)(implicit s: Semigroup[A]): A = s.combine(x, y)
  }

}
