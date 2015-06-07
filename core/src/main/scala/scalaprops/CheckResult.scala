package scalaprops

import scalaprops.CheckResult._
import scalaz.IList

sealed abstract class CheckResult extends Product with Serializable{
  def succeeded: Int
  def discarded: Int

  final def append(that: CheckResult): CheckResult = (this, that) match {
    case (a: Passed, b: Passed) =>
      a.copy(succeeded = a.succeeded + b.succeeded, discarded = a.discarded + b.discarded)
    case (a: Passed, b: Proven) =>
      a.copy(succeeded = a.succeeded + b.succeeded, discarded = a.discarded + b.discarded)
    case (b: Proven, a: Passed) =>
      a.copy(succeeded = a.succeeded + b.succeeded, discarded = a.discarded + b.discarded)
    case (a: Proven, b: Proven) =>
      a.copy(succeeded = a.succeeded + b.succeeded, discarded = a.discarded + b.discarded)
    case ((_: Passed) | (_: Proven), b) =>
      b
    case (a, _) =>
      a
  }
}

object CheckResult {
  final case class Passed(override val succeeded: Int, override val discarded: Int) extends CheckResult
  final case class Proven(override val succeeded: Int, override val discarded: Int) extends CheckResult
  final case class Falsified(override val succeeded: Int, override val discarded: Int, args: IList[Arg]) extends CheckResult
  final case class Exhausted(override val succeeded: Int, override val discarded: Int) extends CheckResult
  final case class PropException(override val succeeded: Int, override val discarded: Int, args: IList[Arg], exception: Throwable) extends CheckResult
  final case class GenException(override val succeeded: Int, override val discarded: Int, exception: Throwable) extends CheckResult
  final case class Timeout(override val succeeded: Int, override val discarded: Int) extends CheckResult
  final case class Ignored(override val succeeded: Int, override val discarded: Int, reason: String) extends CheckResult

  implicit val instance = scalaz.Monoid.instance[CheckResult](_ append _, Passed(0, 0)) // TODO
}
