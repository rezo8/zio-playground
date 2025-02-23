package ch2

import zio.{Random, ZLayer}
import zio.test.*
import zio.test.Assertion.equalTo

object PropertyBasedTesting extends ZIOSpecDefault {
  val intGen: Gen[Any, Int] =
    Gen.int

  /*

    In property-based testing, instead of manually generating inputs and verifying the expected outputs,
     the test framework generates a whole collection of inputs from a distribution of potential inputs you specify.
     It verifies that the expectation holds for all the inputs.

   */

  val testLayer: ZLayer[Any, Nothing, Random with Sized] =
    ZLayer.succeed(Random.RandomLive) ++ Sized.default

  def spec =
    suite("ExampleSpec")(
      test("integer addition is associative") {
        check(intGen, intGen, intGen) { (x, y, z) =>
          val left = (x + y) + z
          val right = x + (y + z)
          assert(left)(equalTo(right))
        }
      },
      test("user generation is stable") { // simple example of generation. Useful for determining values.
        check(genUser) { user =>
          val name = user.name
          val age = user.age
          assert(1)(equalTo(1))
        }
      }.provide(testLayer)
    )

  final case class User(name: String, age: Int)

  val genName: Gen[Random with Sized, String] =
    Gen.asciiString

  val genAge: Gen[Random, Int] =
    Gen.int(18, 120)

  val genUser: Gen[Random with Sized, User] = {
    for {
      name <- genName
      age <- genAge
    } yield User(name, age)
  }
}
