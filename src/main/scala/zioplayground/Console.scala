package zioplayground

object console: 
  type Console = Has[Console.Service]
  
  object Console:
    trait Service:
      def printLine(line: => String): ZIO[Any, Nothing, Unit]
      def getString: ZIO[Any, Nothing, String]
    
    lazy val live: ZLayer[Any, Nothing, Console] =
      ZLayer.succeed(make)

    lazy val make: Service = 
      new:
        def printLine(line: => String) = 
          ZIO.succeed(println(line))

        lazy val getString = 
          ZIO.succeed(scala.io.StdIn.readLine())
  
  def printLine(line: => String): ZIO[Console, Nothing, Unit] = 
    ZIO.accessM(_.get.printLine(line))
    
  def getString: ZIO[Console, Nothing, String] = 
    ZIO.accessM(_.get.getString)

