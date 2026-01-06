package zioplayground


object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  object BusinessLogic:
    trait Service:
      def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[Any, Nothing, Boolean]
    
    lazy val live: ZIO[Google, Nothing, Service] = 
      ZIO.fromFunction(make)

    def make(google: Google): Service =
      new:
        override def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[Any, Nothing, Boolean] = 
          google.countPicturesOf(topic).map(_ % 2 == 0)
    
  def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.doesGoogleHasEvenAmountOfPicturesOn(topic))

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
  lazy val live: ZIO[Any, Nothing, businessLogic.BusinessLogic.Service] =  
    for
      g <- GoogleImpl.live
      bl <- businessLogic.BusinessLogic.live.provide(g)
    yield bl

  lazy val make: businessLogic.BusinessLogic.Service =
    val g = GoogleImpl.make
    val bl = businessLogic.BusinessLogic.make(g)

    bl

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program)
  
  lazy val program = 
    for 
      bl <- DependencyGraph.live
      p  <- makeProgram.provideSome[ZEnv](_ union Has(bl))
    yield p   

  lazy val makeProgram = 
    for
      _ <- console.printLine("-" * 100)

      cats <- businessLogic.doesGoogleHasEvenAmountOfPicturesOn("cats")
      dogs <- businessLogic.doesGoogleHasEvenAmountOfPicturesOn("dogs")

      _ <- console.printLine(cats.toString)
      _ <- console.printLine(dogs.toString)
      _ <- console.printLine("-" * 100)
    yield ()
