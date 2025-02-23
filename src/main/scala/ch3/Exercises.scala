package ch3

import zio.*
import zio.ZIO.some

import java.io.IOException

object Exercises {

  /*
  Using the appropriate effect constructor, fix the following function so
  that it no longer fails with defects when executed.
  Make a note of how the inferred return type for the function changes.
    def failWithMessage(string: String): Task[Nothing] = {
      ZIO.succeed(throw new Error(string))  // This is ZIO[E, Nothing, Nothing] w/o fix.
  }
   */
  def failWithMessage(string: String): Task[Nothing] = {
    ZIO.attempt(throw new Error(string))
  }
  /*
  Using the ZIO #foldCause ZIO operator and the Cause#defectsmethod,
   implement the following function. This function should take the effect,
   inspect defects, and if a suitable defect is found,
   it should recover from the error with the help of the specified function,
   which generates a new success value for such a defect.
   */
  def recoverFromSomeDefects[R, E, A](
    zio: ZIO[R, E, A]
  )(f: Throwable => Option[A]): ZIO[R, E, A] = {
    zio.foldCauseZIO(cause => {
      cause.defects.collectFirst {
        case defect if f(defect).isDefined => f(defect).get
      } match {
        case Some(recoveredValue) => ZIO.succeed(recoveredValue)
        case None                 => ZIO.failCause(cause)
      }
    }, success => ZIO.succeed(success))
  }

  /*

  Using the ZIO#foldCauseZIO operator and the Cause#prettyPrint method,
  implement an operator that takes an effect,
  and returns a new effect that
  logs any failures of the original effect (including errors and defects),
  without changing its failure or success value.
   */
  def logFailures[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] = {
    zio.foldCauseZIO(cause => {
      ZIO.logError(cause.prettyPrint) *> ZIO.failCause(cause)
    }, success => ZIO.succeed(success))
  }

  /*
    Using the ZIO#foldCauseZIO method, which “runs” an effect to an Exit value,
   implement the following function, which will execute the specified effect on any failure at all:
   */

  def onAnyFailure[R, E, A](zio: ZIO[R, E, A],
                            handler: ZIO[R, E, Any]): ZIO[R, E, A] = {
    zio.foldCauseZIO(cause => {
      handler *> ZIO.failCause(cause)
    }, success => ZIO.succeed(success))
  }

  /*
  Using the ZIO#refineOrDie method, implement the ioException function,
  which refines the error channel to only include the IOException error.
   */
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] =
    zio.refineOrDie { case e: IOException => e }

  /*
  Using the ZIO#refineToOrDie method,
  narrow the error type of the following effect to just NumberFormatException.
   */
  val parseNumber: ZIO[Any, NumberFormatException, Int] =
    ZIO.attempt("foo".toInt).refineToOrDie[NumberFormatException]

  /*
   Using the ZIO#foldZIO method, implement the following two functions,
   which make working with Either values easier,
   by shifting the unexpected case into the error channel (and reversing this shifting).
   */
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, B], A] =
    zio.foldZIO(err => ZIO.fail(Left(err)), {
      case Left(a)  => ZIO.succeed(a)
      case Right(b) => ZIO.fail(Right(b))
    })

  def unleft[R, E, A, B](
    zio: ZIO[R, Either[E, B], A]
  ): ZIO[R, E, Either[A, B]] =
    zio.foldZIO({
      case Left(error)          => ZIO.fail(error)
      case Right(eitherSuccess) => ZIO.succeed(Right(eitherSuccess))
    }, success => {
      ZIO.succeed(Left(success))
    })

  /*
    Using the ZIO#foldZIO method, implement the following two functions,
    which make working with Either values easier,
    by shifting the unexpected case into the error channel (and reversing this shifting).
   */

  def right[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.foldZIO(error => {
      ZIO.fail(Left(error))
    }, {
      case Right(success) => ZIO.succeed(success)
      case Left(error)    => ZIO.fail(Right(error))
    })

  def unright[R, E, A, B](
    zio: ZIO[R, Either[E, A], B]
  ): ZIO[R, E, Either[A, B]] =
    zio.foldZIO({
      case Right(eitherLeft) => ZIO.succeed(Left(eitherLeft))
      case Left(error)       => ZIO.fail(error)
    }, success => ZIO.succeed(Right(success)))

  /*
    Using the ZIO#sandbox method, implement the following function.
   */
  def catchAllCause[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
  ): ZIO[R, E2, A] =
    zio.sandbox.foldZIO(err => handler(err), success => ZIO.succeed(success))

  /*
    Using the ZIO#foldCauseZIO method,implement the following function.
   */
  def catchAllCauseZIO[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
  ): ZIO[R, E2, A] = zio.foldCauseZIO(handler(_), ZIO.succeed(_))

}
