package ch2

import ch2.RateLimiterSpec.suite
import zio.*
import zio.Clock.currentTime
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestClock

import java.io.IOException
import java.util
import java.util.concurrent.TimeUnit
import scala.collection.mutable

object CountdownTimer {
  def countdown(n: Int): ZIO[Any, IOException, Unit] = {
    if (n <= 0) ZIO.unit
    else
      for {
        _ <- Console.printLine(n.toString)
        _ <- ZIO.sleep(1.second)
        _ <- countdown(n - 1)
      } yield ()
  }
}

object CountdownTimerTest extends ZIOSpecDefault {
  def spec: Spec[Any, IOException] = suite("Countdown Timer")(
    test("counts down from 5 to 1 with 1-second intervals") {
      for {
        _ <- CountdownTimer.countdown(5) race TestClock.adjust(5.seconds)
        output <- TestConsole.output
      } yield assert(output)(equalTo(Vector("5\n", "4\n", "3\n", "2\n", "1\n")))
    }
  )
}

class ExpiringCache[K, V](ttl: Duration) {
  private val cache = mutable.Map.empty[K, (V, Long)]

  def put(key: K, value: V): ZIO[Any, Nothing, Unit] =
    for {
      currentTime <- currentTime(TimeUnit.MILLISECONDS)
      _ <- ZIO.succeed(cache.put(key, (value, currentTime + ttl.toMillis)))
    } yield ()

  def get(key: K): ZIO[Any, Nothing, Option[V]] =
    for {
      currentTime <- currentTime(TimeUnit.MILLISECONDS)
      result <- ZIO.succeed(
        cache
          .get(key)
          .filter { case (_, expiration) => expiration > currentTime }
          .map(_._1)
      )
    } yield result
}

object ExpiringCacheTest extends ZIOSpecDefault {
  def spec = suite("Expiring Cache")(
    test("stores and retrieves values before expiration") {
      val cache = new ExpiringCache[String, String](5.seconds)
      for {
        _ <- cache.put("key", "value")
        value <- cache.get("key")
      } yield assert(value)(isSome(equalTo("value")))
    },
    test("expires values after the TTL") {
      val cache = new ExpiringCache[String, String](5.seconds)
      for {
        _ <- cache.put("key", "value")
        _ <- TestClock.adjust(6.seconds)
        value <- cache.get("key")
      } yield assert(value)(isNone)
    }
  )
}

class RateLimiter(opsPerMinute: Int) {

  var allOpTimes: List[Long] = List()

  def operation(): ZIO[Any, Throwable, Unit] = {
    for {
      currTime <- currentTime(TimeUnit.MILLISECONDS)
      _ <- canRunOperation(currTime)
      operationTime <- runOperation()
      // TODO potentially clean list here.
    } yield ()
  }

  private def canRunOperation(currTime: Long): ZIO[Any, Throwable, Unit] = {
    val foundTimes = allOpTimes.filter(oldTime => {
      (currTime - oldTime - 60.seconds.toMillis) < 0
    })
    if (foundTimes.size >= opsPerMinute) {
      ZIO.fail(new Throwable("eclipsed rate limit"))
    } else {
      ZIO.unit
    }
  }

  private def runOperation() = {
    for {
      _ <- Console.printLine("operation!")
      currTime <- currentTime(TimeUnit.MILLISECONDS)
      _ = allOpTimes = allOpTimes :+ currTime
    } yield currTime
  }
}

object RateLimiterSpec extends ZIOSpecDefault {
  def spec =
    suite("RateLimiter")(
      test(
        "runs into rate limiter when it executes more than 5 times in a minute"
      ) {
        val rateLimiter = new RateLimiter(5)
        for {
          _ <- ZIO.foreach(1 to 5)(_ => rateLimiter.operation())
          result <- rateLimiter
            .operation()
            .exit // Capture the result as an Exit
        } yield
          assert(result)(
            fails(
              isSubtype[Throwable](hasMessage(equalTo("eclipsed rate limit")))
            )
          )
      },
      test("successfully runs operation without fail when it waits a minute") {
        val rateLimiter = new RateLimiter(5)
        for {
          _ <- ZIO.foreach(1 to 5)(_ => rateLimiter.operation())
          _ <- TestClock.adjust(60.seconds)
          _ <- rateLimiter.operation()
          output <- TestConsole.output()
        } yield assert(output.size)(equalTo(6))
      },
    )
}

object ListReverser {
  def reverseList[A](toReverse: List[A]): Task[List[A]] = {
    ZIO.attempt(toReverse.reverse)
  }
}

object ListReverserSpec extends ZIOSpecDefault {
  def spec =
    suite("ListReverse")(
      test("reversing a list twice returns the original list") {
        check(Gen.listOf(Gen.int)) { list =>
          for {
            reversedOnce <- ListReverser.reverseList(list)
            reversedTwice <- ListReverser.reverseList(reversedOnce)
          } yield {
            assertTrue((reversedOnce == list.reverse) && reversedTwice == list)
          }
        }
      }
    )
}

sealed trait AVLTree[+A]

case object Empty extends AVLTree[Nothing]

case class Node[A](value: A, left: AVLTree[A], right: AVLTree[A], height: Int)
    extends AVLTree[A]

object AVLTree {
  def height[A](tree: AVLTree[A]): Int = tree match {
    case Empty            => 0
    case Node(_, _, _, h) => h
  }

  def makeNode[A](value: A, left: AVLTree[A], right: AVLTree[A]): Node[A] =
    Node(value, left, right, Math.max(height(left), height(right)) + 1)

  def rotateRight[A](node: Node[A]): Node[A] = node match {
    case Node(v, Node(lv, ll, lr, _), r, _) =>
      makeNode(lv, ll, makeNode(v, lr, r))
    case _ => node // Should not happen
  }

  def rotateLeft[A](node: Node[A]): Node[A] = node match {
    case Node(v, l, Node(rv, rl, rr, _), _) =>
      makeNode(rv, makeNode(v, l, rl), rr)
    case _ => node // Should not happen
  }

  def balanceFactor[A](node: Node[A]): Int =
    height(node.left) - height(node.right)

  def balance[A](tree: AVLTree[A]): AVLTree[A] = tree match {
    case Empty => Empty
    case node: Node[A] =>
      val bf = balanceFactor(node)
      if (bf > 1) {
        if (balanceFactor(node.left.asInstanceOf[Node[A]]) < 0) {
          rotateRight(
            makeNode(
              node.value,
              rotateLeft(node.left.asInstanceOf[Node[A]]),
              node.right
            )
          )
        } else {
          rotateRight(node)
        }
      } else if (bf < -1) {
        if (balanceFactor(node.right.asInstanceOf[Node[A]]) > 0) {
          rotateLeft(
            makeNode(
              node.value,
              node.left,
              rotateRight(node.right.asInstanceOf[Node[A]])
            )
          )
        } else {
          rotateLeft(node)
        }
      } else {
        node // Already balanced
      }
  }

  // Insert a value into the tree
  def insert[A](tree: AVLTree[A],
                value: A)(implicit ord: Ordering[A]): AVLTree[A] = {
    def go(t: AVLTree[A]): AVLTree[A] = t match {
      case Empty => makeNode(value, Empty, Empty)
      case node: Node[A] =>
        if (ord.lt(value, node.value)) {
          balance(makeNode(node.value, go(node.left), node.right))
        } else if (ord.gt(value, node.value)) {
          balance(makeNode(node.value, node.left, go(node.right)))
        } else {
          t
        }
    }

    go(tree)
  }

  def delete[A](tree: AVLTree[A],
                value: A)(implicit ord: Ordering[A]): AVLTree[A] = {
    def go(t: AVLTree[A]): AVLTree[A] = t match {
      case Empty => Empty
      case node: Node[A] =>
        if (ord.lt(value, node.value)) {
          balance(makeNode(node.value, go(node.left), node.right))
        } else if (ord.gt(value, node.value)) {
          balance(makeNode(node.value, node.left, go(node.right)))
        } else {
          (node.left, node.right) match {
            case (Empty, Empty) => Empty
            case (Empty, _)     => node.right
            case (_, Empty)     => node.left
            case _ =>
              val minNode = findMin(node.right)
              balance(
                makeNode(
                  minNode.value,
                  node.left,
                  delete(node.right, minNode.value)
                )
              )
          }
        }
    }
    go(tree)
  }

  def findMin[A](tree: AVLTree[A]): Node[A] = tree match {
    case Node(v, Empty, _, _) => Node(v, Empty, Empty, 1)
    case Node(_, l, _, _)     => findMin(l)
    case _                    => throw new NoSuchElementException("Tree is empty")
  }

  def isBalanced[A](tree: AVLTree[A]): Boolean = tree match {
    case Empty => true
    case Node(_, l, r, _) =>
      Math.abs(height(l) - height(r)) <= 1 && isBalanced(l) && isBalanced(r)
  }
}

object AVLTreeSpec extends ZIOSpecDefault {

  def spec =
    suite("AVLTreeSpec")(test("insert maintains balance") {
      check(Gen.listOf(Gen.int)) { values =>
        val tree = values.foldLeft(Empty: AVLTree[Int])(AVLTree.insert)
        assert(AVLTree.isBalanced(tree))(isTrue)
      }
    }, test("delete maintains balance") {
      check(Gen.listOf(Gen.int), Gen.int) { (values, toDelete) =>
        val tree = values.foldLeft(Empty: AVLTree[Int])(AVLTree.insert)
        val afterDelete = AVLTree.delete(tree, toDelete)
        assert(AVLTree.isBalanced(afterDelete))(isTrue)
      }
    })
}
