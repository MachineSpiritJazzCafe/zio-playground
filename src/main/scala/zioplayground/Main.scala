package zioplayground

import java.io.IOException


object businessLogic:
  type BusinessLogic = Has[BusinessLogic.Service]

  object BusinessLogic:
    trait Service:
      def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[Any, Nothing, Boolean]
    
    lazy val live: ZLayer[google.Google, Nothing, BusinessLogic] = 
      ZLayer.fromService(make)

    def make(g: google.Google.Service): Service =
      new:
        override def doesGoogleHasEvenAmountOfPicturesOn(
          topic: String
        ): ZIO[Any, Nothing, Boolean] = 
        g.countPicturesOf(topic).map(_ % 2 == 0)
    
  def doesGoogleHasEvenAmountOfPicturesOn(topic: String): ZIO[BusinessLogic, Nothing, Boolean] =
    ZIO.accessM(_.get.doesGoogleHasEvenAmountOfPicturesOn(topic))

object google:
  type Google = Has[Google.Service]
  object Google:
    trait Service:
      def countPicturesOf(topic: String): ZIO[Any, Nothing, Int]

  def countPicturesOf(topic: String): ZIO[Google, Nothing, Int] = 
    ZIO.accessM(_.get.countPicturesOf(topic))

object GoogleImpl:
  lazy val live: ZLayer[Any, Nothing, google.Google] = 
    ZLayer.succeed(make)

  lazy val make: google.Google.Service =
    new:
      override def countPicturesOf(topic: String): ZIO[Any, Nothing, Int] = 
        ZIO.succeed(if (topic == "cats") 1337 else 1338)

object controller:
  type Controller = Has[Controller.Service]
  object Controller:
    trait Service:
      def run: ZIO[Any, IOException, Unit]

    lazy val live: ZLayer[businessLogic.BusinessLogic & console.Console, Nothing, Controller] =
      ZLayer.fromServices(make)
  
    def make(bl: businessLogic.BusinessLogic.Service, con: console.Console.Service): Service =
      new:
        override lazy val run: ZIO[Any, IOException, Unit] = 
          for
            _ <- con.printLine("-" * 100)

            cats <- bl.doesGoogleHasEvenAmountOfPicturesOn("cats")
            dogs <- bl.doesGoogleHasEvenAmountOfPicturesOn("dogs")

            _ <- con.printLine(cats.toString)
            _ <- con.printLine(dogs.toString)
            _ <- con.printLine("-" * 100)
          yield ()

  lazy val run: ZIO[Controller, IOException, Unit] =
    ZIO.accessM(_.get.run)

object DependencyGraph:
  lazy val env: ZLayer[Any, Nothing, controller.Controller] =
    GoogleImpl.live >>> businessLogic.BusinessLogic.live ++
      console.Console.live >>>
      controller.Controller.live

object Main extends scala.App:
  Runtime.default.unsafeRunSync(program)

  lazy val program =
    controller.run.provideLayer(DependencyGraph.env)

