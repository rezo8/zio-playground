package ch14_specs

import ch14to16_resource_management.ch14_acquire_release.Example.ZIOHomeMade
import zio.*
import zio.test.*
import zio.test.Assertion.*

import java.util.concurrent.atomic.AtomicBoolean

object AcquireReleaseWithSpec extends ZIOSpecDefault {
  def spec = suite("acquireReleaseWith guarantees")(
    test("acquire action is uninterruptible") {
      val acquireFlag = new AtomicBoolean(false)
      val acquire = ZIO.succeed(acquireFlag.set(true)).uninterruptible
      val release = ZIO.unit
      val use = ZIO.unit

      for {
        _ <- ZIOHomeMade.acquireReleaseWith(acquire)(_ => release)(_ => use)
      } yield assert(acquireFlag.get())(isTrue)
    },
    test("release action is uninterruptible") {
      val releaseFlag = new AtomicBoolean(false)
      val acquire = ZIO.unit
      val release = ZIO.succeed(releaseFlag.set(true)).uninterruptible
      val use = ZIO.unit

      for {
        _ <- ZIOHomeMade.acquireReleaseWith(acquire)(_ => release)(_ => use)
      } yield assert(releaseFlag.get())(isTrue)
    },
    test("release is executed after use, regardless of how use completes") {
      val acquire = ZIO.unit
      val releaseFlag = new AtomicBoolean(false)
      val release = ZIO.succeed(releaseFlag.set(true))
      val use = ZIO.fail(new RuntimeException("use failed")).orElse(ZIO.unit)

      for {
        _ <- ZIOHomeMade.acquireReleaseWith(acquire)(_ => release)(_ => use)
      } yield assert(releaseFlag.get())(isTrue)
    }
  )
}
