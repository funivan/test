import scala.collection.immutable.HashMap

case class Instructions(i: HashMap[String, Operation] = HashMap()) {
  def +(tuple: (String, Operation)) : Instructions = Instructions(i + tuple)
}
