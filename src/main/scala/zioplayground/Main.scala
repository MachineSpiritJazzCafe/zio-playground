package zioplayground

import myzio._ 

object MainApp extends scala.App:
  Runtime.default.unsafeRunSync(run)

  lazy val run = 
    for 
      _ <- Console.printLine("-" * 10)
      _ <- Console.printLine("What's your name?")
      name <- ZIO.succeed("Mouse Mikey")
      _ <- Console.printLine(s"Hello $name!")
      _ <- Console.printLine("-" * 10)
    yield ()

