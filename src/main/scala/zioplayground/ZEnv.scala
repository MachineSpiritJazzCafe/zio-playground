package zioplayground

type ZEnv = console.Console // Usually with Clock, System, Random, Blocking
object ZEnv:
  lazy val live: ZLayer[Any, Nothing, ZEnv] = 
    console.Console.live
