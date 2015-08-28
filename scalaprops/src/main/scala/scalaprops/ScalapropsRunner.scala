package scalaprops

import java.lang.Thread.UncaughtExceptionHandler
import java.util.concurrent.{TimeoutException, ForkJoinPool}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import sbt.testing._
import scala.util.control.NonFatal
import scalaz._
import org.scalajs.testinterface.TestUtils

object ScalapropsRunner {

  private def logger(loggers: Array[Logger]): Logger = new Logger {
    override def warn(msg: String): Unit =
      loggers.foreach(_.warn(msg))
    override def error(msg: String): Unit =
      loggers.foreach(_.error(msg))
    override def ansiCodesSupported(): Boolean =
      loggers.forall(_.ansiCodesSupported())
    override def debug(msg: String): Unit =
      loggers.foreach(_.debug(msg))
    override def trace(t: Throwable): Unit =
      loggers.foreach(_.trace(t))
    override def info(msg: String): Unit =
      loggers.foreach(_.info(msg))
  }

}

final class ScalapropsRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

  private[this] val successCount = new AtomicInteger
  private[this] val failureCount = new AtomicInteger
  private[this] val errorCount = new AtomicInteger
  private[this] val ignoredCount = new AtomicInteger
  private[this] val testCount = new AtomicInteger

  private[this] val taskdef2task: TaskDef => sbt.testing.Task = { taskdef =>
    val testClassName = taskdef.fullyQualifiedName()
    val emptyThrowable = new OptionalThrowable

    new sbt.testing.Task {
      override def taskDef() = taskdef

      override def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
        continuation(execute(eventHandler, loggers))
      }

      override def execute(eventHandler: EventHandler, loggers: Array[Logger]) = {
        val log = ScalapropsRunner.logger(loggers)

        lazy val executorService: ForkJoinPool = new ForkJoinPool(
          sys.runtime.availableProcessors(),
          ForkJoinPool.defaultForkJoinWorkerThreadFactory,
          new UncaughtExceptionHandler {
            def uncaughtException(t: Thread, e: Throwable): Unit = {
              log.error("uncaughtException Thread = " + t)
              log.trace(e)
              e.printStackTrace()
              executorService.shutdown()
            }
          },
          false
        )

        try {
          val obj = taskDef.fingerprint() match {
            case fingerprint: SubclassFingerprint if fingerprint.superclassName() == "scalaprops.Scalaprops" =>
              if (fingerprint.isModule) {
                TestUtils.loadModule(testClassName, testClassLoader) match {
                  case m : Scalaprops => m
                  case x => throw new Exception(s"Cannot test $taskDef of type: $x")
                }
              }
              else {
                throw new Exception("FunSuite only works on objects, classes don't work.")
              }
            case _ => throw new Exception("can not find scalaporps.Scalaprops instance.")
          }
          val test = Properties.noSort[Any](
            Tree.node(
              testClassName -> Maybe.empty,
              obj.transformProperties(obj.props.toList).map(_.props)(collection.breakOut)
            )
          )
          val result = test.props.map { case (id, checkOpt) =>
            val name = id.toString // TODO create type class ?
            (id: Any) -> (checkOpt match{
              case Maybe.Just(check) => LazyOption.lazySome{
                val cancel = new AtomicBoolean(false)
                val selector = new TestSelector(name)
                def event(status: Status, duration: Long, result0: Throwable \&/ CheckResult) = {
                  val err = result0.a match {
                    case Some(e) => new OptionalThrowable(e)
                    case None => emptyThrowable
                  }
                  ScalapropsEvent(testClassName, taskdef.fingerprint(), selector, status, err, duration, result0)
                }

                val param = check.paramEndo(obj.param)
                val start = System.currentTimeMillis()
                val r = try {
                  obj.listener.onStart(obj, name, check.prop, param, log)
                  val r = scalaz.concurrent.Task(
                    check.prop.check(
                      param,
                      cancel,
                      count => obj.listener.onCheck(obj, name, check, log, count)
                    )
                  )(executorService).runFor(param.timeout)
                  val duration = System.currentTimeMillis() - start
                  obj.listener.onFinish(obj, name, check.prop, param, r, log)
                  r match {
                    case _: CheckResult.Proven | _: CheckResult.Passed =>
                      successCount.incrementAndGet()
                      event(Status.Success, duration, \&/.That(r))
                    case _: CheckResult.Exhausted | _: CheckResult.Falsified =>
                      failureCount.incrementAndGet()
                      event(Status.Failure, duration, \&/.That(r))
                    case e: CheckResult.GenException =>
                      log.trace(e.exception)
                      errorCount.incrementAndGet()
                      event(Status.Error, duration, \&/.Both(e.exception, r))
                    case e: CheckResult.PropException =>
                      log.trace(e.exception)
                      errorCount.incrementAndGet()
                      event(Status.Error, duration, \&/.Both(e.exception, r))
                    case e: CheckResult.Timeout =>
                      errorCount.incrementAndGet()
                      event(Status.Error, duration, \&/.That(r))
                    case e: CheckResult.Ignored =>
                      ignoredCount.incrementAndGet()
                      event(Status.Ignored, duration, \&/.That(r))
                  }
                } catch {
                  case e: TimeoutException =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    errorCount.incrementAndGet()
                    event(Status.Error, duration, \&/.This(e))
                  case NonFatal(e) =>
                    val duration = System.currentTimeMillis() - start
                    log.trace(e)
                    obj.listener.onError(obj, name, e, log)
                    errorCount.incrementAndGet()
                    event(Status.Error, duration, \&/.This(e))
                } finally {
                  cancel.set(true)
                  testCount.incrementAndGet()
                }
                eventHandler.handle(r)
                (check.prop, param, r)
              }
              case Maybe.Empty() =>
                LazyOption.lazyNone
            })
          }
          obj.listener.onFinishAll(obj, result, log)
          Array()
        } finally {
          executorService.shutdown()
        }
      }

      override def tags() = Array()
    }
  }

  override def tasks(taskDefs: Array[TaskDef]) = taskDefs.map(taskdef2task)

  override def done() = Seq(
    s"Total test count: $testCount",
    s"Failed $failureCount, Errors $errorCount, Passed $successCount, Ignored $ignoredCount"
  ).map(Console.CYAN + _).mkString(sys.props("line.separator"))

  override def receiveMessage(msg: String) = None

  override def serializeTask(task: sbt.testing.Task, serializer: TaskDef => String) =
    serializer(task.taskDef())

  override def deserializeTask(task: String, deserializer: String => TaskDef) =
    taskdef2task(deserializer(task))
}
