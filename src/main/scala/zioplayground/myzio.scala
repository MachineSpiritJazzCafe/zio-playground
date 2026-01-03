package zioplayground

object myzio:
  final case class ZIO[A](thunk: () => A):
    def flatMap[B](azb: A => ZIO[B]): ZIO[B] = 
      ZIO.succeed {
        val a = thunk()

        val zb = azb(a)
        val b = zb.thunk()

        b
      }

    def map[B](ab: A => B): ZIO[B] =
      ZIO.succeed {
        val a = thunk()

        val b = ab(a)
        
        b
      }
  
  object ZIO:
    def succeed[A](a: => A): ZIO[A] =
      ZIO(() => a)

  object Console:
    def printLine(line: => String) = 
      ZIO.succeed(println(line))

    val getString = 
      ZIO.succeed(scala.io.StdIn.readLine())

  object Runtime:
    object default:
      def unsafeRunSync[A](zio: => ZIO[A]): A =
        zio.thunk()

      
