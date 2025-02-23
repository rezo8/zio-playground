package ch2

import zio.ZIO
import zio.test.*
import zio.test.Assertion.*

object ExampleSpec extends ZIOSpecDefault {

  def spec =
    suite("ExampleSpec")(
      test("testing an effect using map operator") {
        ZIO.succeed(1 + 1).map(n => assert(n)(equalTo(2)))
      },
      test("testing an effect using a for comprehension") {
        for {
          n <- ZIO.succeed(1 + 1)
        } yield assert(n)(equalTo(2))
      },
      test("hasSameElement") {
        assert(List(1, 1, 2, 3))(hasSameElements(List(3, 2, 1, 1)))
      },
      test("fails") {
        for {
          exit <- ZIO.attempt(1 / 0).catchAll(_ => ZIO.fail(())).exit
        } yield assert(exit)(fails(isUnit))
      }
    )

}
