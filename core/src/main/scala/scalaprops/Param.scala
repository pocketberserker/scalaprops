package scalaprops

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.Duration
import scalaz.Endo

final case class Param(
  parallel: Int = 1,
  rand: Rand,
  minSuccessful: Int = 100,
  maxDiscarded: Int = 500,
  minSize: Int = 0,
  maxSize: Int = Param.defaultSize,
  timeout: Duration = Duration(30, TimeUnit.SECONDS)
) {

  def list: List[Param] = {
    if(parallel < 0){
      sys.error("invalid parallel value " + parallel)
    }else if(parallel == 1){
      List(this)
    }else{
      val s = minSuccessful / parallel
      val d = maxDiscarded / parallel
      val t = timeout / parallel
      // TODO rand
      List.fill(parallel)(this.copy(
        minSuccessful = s,
        maxDiscarded = d,
        timeout = t
      ))
    }
  }

}

object Param {
  val defaultSize = 100

  def withCurrentTimeSeed(): Param = Param(
    rand = Rand.standard(System.nanoTime())
  )

  def rand(rand: Rand): Endo[Param] =
    Endo(_.copy(rand = rand))

  def constantSeed(value: Int): Endo[Param] =
    Endo(_.copy(rand = Rand.fromSeed(value)))

  def minSuccessful(n: Int): Endo[Param] =
    Endo(_.copy(minSuccessful = n))

  def maxSize(n: Int): Endo[Param] =
    Endo(_.copy(maxSize = n))

  def timeout(n: Int, timeunit: TimeUnit): Endo[Param] =
    Endo(_.copy(timeout = Duration(n, timeunit)))

  val id: Endo[Param] =
    Endo.idEndo[Param]
}
