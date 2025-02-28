package ch9to13_concurrent_structures.ch12_broadcasting

import zio.*
import zio.http.Header.IfRange.DateTime
import zio.nio.file.{Files, Path}

import java.nio.charset.StandardCharsets
import java.nio.file.StandardOpenOption
import java.time.{Instant, OffsetDateTime}
import java.util.concurrent.TimeUnit

object Exercises extends ZIOAppDefault {

  sealed trait ChatEvent

  case class Message(from: String, text: String) extends ChatEvent

  case object UserLeft extends ChatEvent

  trait UserSession {
    def sendMessage(message: String): UIO[Unit]

    def receiveMessages: ZIO[Scope, Nothing, Dequeue[ChatEvent]]

    def leave: UIO[Unit]
  }

  object UserSession {
    def make(username: String,
             hub: Hub[ChatEvent]): ZIO[Any, Nothing, UserSession] = {
      for {
        receiversRef <- Ref.make(Set[Dequeue[ChatEvent]]())
        isOnRef <- Ref.make(true)
        userSession = new UserSession {
          override def sendMessage(message: String): UIO[Unit] = {
            for {
              isOn <- isOnRef.get
              res <- if (isOn) {
                hub.offer(Message(username, message)).unit
              } else {
                ZIO.unit
              }
            } yield res
          }

          override def receiveMessages
            : ZIO[Scope, Nothing, Dequeue[ChatEvent]] = {
            for {
              receiver <- hub.subscribe
              _ <- receiversRef.modify(oldReceivers => {
                ((), oldReceivers + receiver)
              })
            } yield receiver
          }

          override def leave: UIO[Unit] = {
            for {
              receivers <- receiversRef.get
              _ <- ZIO.foreach(receivers)(_.shutdown)
              _ <- isOnRef.modify(_ => ((), false))
            } yield ()
          }
        }
      } yield userSession
    }
  }

  trait ChatRoom {
    def join(username: String): ZIO[Scope, Nothing, UserSession]
    def shutdown(username: String): UIO[Unit]
  }

  object ChatRoom {
    def make: URIO[Scope, ChatRoom] =
      for {
        hub <- Hub.unbounded[ChatEvent]
        userMapsRef <- Ref.make(Map[String, UserSession]())
        logFile = Path("chat_log.txt")
        _ <- hub.subscribe.flatMap { writeProcess =>
          (writeProcess.take
            .tap(event => ZIO.logInfo(s"Processing event: ${event.toString}"))
//            .flatMap { event =>
//              for {
//                _ <- ZIO.logInfo(
//                  s"Writing event to log file: ${event.toString}"
//                )
//                // Append the event to the log file
//                _ <- Files
//                  .writeLines(
//                    logFile,
//                    Seq(event.toString),
//                    openOptions =
//                      Set(StandardOpenOption.CREATE, StandardOpenOption.APPEND)
//                  )
//                  .catchAll(
//                    error =>
//
//                      ZIO.logError(
//                        s"Failed to write to log file: ${error.getMessage}"
//                    )
//                  )
//                _ <- ZIO.logInfo(
//                  s"Successfully wrote event to log file: ${event.toString}"
//                )
//              } yield ()
//            }
            .forever
            .forkScoped // Ensure the fiber is properly scoped
          )
        }
      } yield
        new ChatRoom {
          override def join(
            username: String
          ): ZIO[Scope, Nothing, UserSession] = {
            for {
              receiversRef <- Ref.make(Set[Dequeue[ChatEvent]]())
              userSession <- UserSession.make(username, hub)
              _ <- userMapsRef.modify(x => ((), x + (username -> userSession))) // TODO handle collision with exception maybe?
            } yield userSession
          }

          def shutdown(username: String): UIO[Unit] = {
            for {
              userMaps <- userMapsRef.get
              userProcs = userMaps.get(username)
              leaveRes <- userProcs.fold(ZIO.succeed(()))(
                exists => exists.leave
              )
              _ <- userMapsRef.modify(x => ((), x.removed(username)))
            } yield leaveRes
          }
        }
  }

  // Something is off with the chat room :(.
  // Struggling to fix and have it match the interface provided in the book.

  val chatRoomProgram =
    for {
      chatRoom <- ChatRoom.make
      user1 <- chatRoom.join("Alice")
      user2 <- chatRoom.join("Bob")
      receiver <- user1.receiveMessages
      _ <- receiver.take
        .flatMap(x => {
          ZIO.logInfo(x.toString)
        })
        .forever
        .fork
      _ <- user1.sendMessage("hello")
      _ <- user2.sendMessage("hey there")
      _ <- chatRoom.shutdown("Bob")
      _ <- user2.sendMessage("oh god no")
      _ <- user1.sendMessage("Bob! What happened. Bob can you hear me?")
      _ <- user2.sendMessage("It feels like a dream.")

    } yield ()

  case class Bid(auctionId: String,
                 bidderId: String,
                 amount: BigDecimal,
                 timestamp: Long)

  case class AuctionState(id: String,
                          currentPrice: BigDecimal,
                          currentWinner: Option[String],
                          endTime: Long,
                          isActive: Boolean)

  sealed trait AuctionEvent

  case class BidPlaced(bid: Bid) extends AuctionEvent

  case class AuctionEnded(auctionId: String,
                          finalPrice: BigDecimal,
                          winner: Option[String])
      extends AuctionEvent

  trait AuctionSystem {
    def placeBid(auctionId: String,
                 bidderId: String,
                 amount: BigDecimal): UIO[Boolean]

    def createAuction(id: String,
                      startPrice: BigDecimal,
                      duration: Duration): ZIO[Any, Exception, Unit]

    def subscribe: ZIO[Scope, Nothing, Dequeue[AuctionEvent]]

    def getAuction(id: String): UIO[Option[AuctionState]]
  }
//
//  object AuctionSystem {
//    // Define the live implementation as a ZLayer
//    val live: ZLayer[Any, Nothing, AuctionSystem] = ZLayer {
//      for {
//        stateRef <- Ref.make(State(Map.empty, List.empty))
//      } yield AuctionSystemLive(stateRef)
//    }
//
//    private case class State(auctions: Map[String, AuctionState],
//                             subscribers: List[Queue[AuctionEvent]])
//
//    private case class AuctionSystemLive(stateRef: Ref[State])
//        extends AuctionSystem {
//
//      override def placeBid(auctionId: String,
//                            bidderId: String,
//                            amount: BigDecimal): UIO[Boolean] = {
//        for {
//          now <- Clock.currentTime(TimeUnit.MILLISECONDS)
//          bid = Bid(auctionId, bidderId, amount, now)
//          (success, event) <- stateRef.modify { state =>
//            state.auctions.get(auctionId) match {
//              case Some(auction)
//                  if auction.isActive && amount > auction.currentPrice && now < auction.endTime =>
//                val updatedAuction = auction.copy(
//                  currentPrice = amount,
//                  currentWinner = Some(bidderId)
//                )
//                val updatedAuctions = state.auctions + (auctionId -> updatedAuction)
//                val newState = state.copy(auctions = updatedAuctions)
//                val event = BidPlaced(bid)
//                (( event), newState)
//              case _ =>
//                ((false, null), state)
//            }
//          }
//          _ <- ZIO.foreachDiscard(state.subscribers)(
//            _.offer(event).when(success)
//          )
//        } yield success
//      }
//
//      override def createAuction(
//        id: String,
//        startPrice: BigDecimal,
//        duration: Duration
//      ): ZIO[Any, Exception, Unit] = {
//        for {
//          now <- Clock.currentTime(TimeUnit.MILLISECONDS)
//          endTime = now + duration.toMillis
//          auction = AuctionState(id, startPrice, None, endTime, isActive = true)
//          _ <- stateRef.update(
//            state => state.copy(auctions = state.auctions + (id -> auction))
//          )
//          _ <- ZIO.sleep(duration) *> endAuction(id)
//        } yield ()
//      }
//
//      private def endAuction(auctionId: String): UIO[Unit] = {
//        for {
//          state <- stateRef.get
//          _ <- state.auctions.get(auctionId) match {
//            case Some(auction) if auction.isActive =>
//              val updatedAuction = auction.copy(isActive = false)
//              val updatedAuctions = state.auctions + (auctionId -> updatedAuction)
//              val event = AuctionEnded(
//                auctionId,
//                updatedAuction.currentPrice,
//                updatedAuction.currentWinner
//              )
//              stateRef.set(state.copy(auctions = updatedAuctions)) *>
//                ZIO.foreachDiscard(state.subscribers)(_.offer(event))
//            case _ => ZIO.unit
//          }
//        } yield ()
//      }
//
//      override def subscribe: ZIO[Scope, Nothing, Dequeue[AuctionEvent]] = {
//        for {
//          queue <- Queue.unbounded[AuctionEvent]
//          _ <- stateRef.update(
//            state => state.copy(subscribers = queue :: state.subscribers)
//          )
//        } yield queue
//      }
//
//      override def getAuction(id: String): UIO[Option[AuctionState]] = {
//        stateRef.get.map(_.auctions.get(id))
//      }
//    }
//  }

  val auctionProgram = for {
    system <- ZIO.service[AuctionSystem]
    _ <- system.createAuction("auction1", BigDecimal(100), 10.seconds)
    _ <- system.placeBid("auction1", "bidder1", BigDecimal(150))
    _ <- system.placeBid("auction1", "bidder2", BigDecimal(200))
    _ <- ZIO.sleep(15.seconds)
    state <- system.getAuction("auction1")
    _ <- ZIO.debug(s"Auction state: $state")
  } yield ()

  override def run = {
    for {

      _ <- ZIO.logInfo("Chat Room Program")
      _ <- chatRoomProgram
//      _ <- ZIO.logInfo("Auction Program")
//      _ <- auctionProgram
    } yield 1
  }
}
