package zioplayground

// import zio._

// object MainApp extends scala.App:
//   println(Runtime.default.unsafeRunSync(run))
//
//   lazy val run = 
//     for 
//       _ <- Console.printLine("-" * 10)
//       _ <- Console.printLine("What's your name?")
//       name <- ZIO.succeed("Mouse Mikey")
//       _ <- Console.printLine(s"Hello $name!")
//
//     //  _ <- ZIO
//     //    .attempt(throw RuntimeException("boom!"))
//     //    .mapError(_.getMessage)
//     //    .catchAll(_ => ZIO.succeed("naah, we good"))
//       _ <- Console.printLine("-" * 10)
//     yield ()
//
//
//

object businessLogic:
  trait BusinessLogic:
    def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[Any, Nothing, Boolean]

  object BusinessLogic:
    lazy val live: ZIO[Google, Nothing, BusinessLogic] = 
      ZIO.fromFunction(make)

    def make(google: Google): BusinessLogic =
      new:
        override def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[Any, Nothing, Boolean] = 
          google.countPicturesOf(topic).map(_ % 2 == 0)
    
  def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.doesGoogleHasEvenAmountOfPicturesOn(topic))

trait Google:
  def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

object GoogleImpl:
  lazy val live: ZIO[Any, Nothing, Google] = 
    ZIO.succeed(make)

  lazy val make: Google =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] = 
        ZIO.succeed(if (topic == "cats") 1337 else 1338)

object DependencyGraph:
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic] =  
    for
      g <- GoogleImpl.live
      bl <- businessLogic.BusinessLogic.live.provide(g)
    yield bl

  lazy val make: businessLogic.BusinessLogic =
    val g = GoogleImpl.make
    val bl = businessLogic.BusinessLogic.make(g)

    bl

object Main extends scala.App:
  println(Runtime.default.unsafeRunSync(program.provide(DependencyGraph.make)))
  

  lazy val program = 
    for 
      _ <- Console.printLine("-" * 100)

      cats <- businessLogic.doesGoogleHasEvenAmountOfPicturesOn("cats")
      dogs <- businessLogic.doesGoogleHasEvenAmountOfPicturesOn("dogs")

      _ <- Console.printLine(cats.toString)
      _ <- Console.printLine(dogs.toString)
      _ <- Console.printLine("-" * 100)
    yield ()  
