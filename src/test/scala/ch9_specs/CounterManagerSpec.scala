package ch9_specs

import zio.test.*
import ch9to13_concurrent_structures.ch9.Exercises.CounterManager
import zio.test.Assertion.equalTo

object CounterManagerSpec extends ZIOSpecDefault {

  // Test suite
  def spec = suite("CounterManagerSpec")(
    test("increment should increase the counter by 1") {
      for {
        manager <- CounterManager.make
        _ <- manager.increment("counter1")
        value <- manager.get("counter1")
      } yield assert(value)(equalTo(1L))
    },
    test("decrement should decrease the counter by 1") {
      for {
        manager <- CounterManager.make
        _ <- manager.increment("counter1")
        _ <- manager.increment("counter1")
        _ <- manager.decrement("counter1")
        value <- manager.get("counter1")
      } yield assert(value)(equalTo(1L))
    },
    test("get should return the current value of the counter") {
      for {
        manager <- CounterManager.make
        _ <- manager.increment("counter1")
        _ <- manager.increment("counter1")
        value <- manager.get("counter1")
      } yield assert(value)(equalTo(2L))
    },
    test("reset should set the counter value to 0") {
      for {
        manager <- CounterManager.make
        _ <- manager.increment("counter1")
        _ <- manager.reset("counter1")
        value <- manager.get("counter1")
      } yield assert(value)(equalTo(0L))
    },
    test("remove should delete the counter") {
      for {
        manager <- CounterManager.make
        _ <- manager.increment("counter1")
        _ <- manager.remove("counter1")
        value <- manager.get("counter1")
      } yield
        assert(value)(equalTo(0L)) // Assuming get returns 0 for a non-existent counter
    }
  )
}
