package zioplayground

import zio._ 

object MainApp extends ZIOAppDefault:
  def run = 
    for 
      _ <- Console.printLine("-" * 10)
      _ <- Console.printLine("What's your name?")
      name <- ZIO.succeed("Mouse Mikey")
      _ <- Console.printLine(s"Hello $name!")
      _ <- Console.printLine("-" * 10)
    yield ()

