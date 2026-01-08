package zioplayground

import java.io.IOException

object console: 
  type Console = Has[Console.Service]
  
  object Console:
    trait Service:
      def printLine(line: => String): ZIO[Any, IOException, Unit]
      def getString: ZIO[Any, IOException, String]
    
    lazy val live: ZLayer[Any, Nothing, Console] =
      ZLayer.succeed(make)

    lazy val make: Service = 
      new:
        def printLine(line: => String): ZIO[Any, IOException, Unit] = 
          ZIO.succeed(println(line))

        lazy val getString: ZIO[Any, IOException, String] = 
          ZIO.succeed(scala.io.StdIn.readLine())
  
  def printLine(line: => String): ZIO[Console, IOException, Unit] = 
    ZIO.accessM(_.get.printLine(line))
    
  def getString: ZIO[Console, IOException, String] = 
    ZIO.accessM(_.get.getString)

