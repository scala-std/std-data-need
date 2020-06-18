package std.data

import org.scalatest.PropSpec

class NeedSpec extends PropSpec {
  property("fib") {
    def fib(n: Int): Need[BigInt] =
      if (n <= 1) Need.now(BigInt(1))
      else for {
        a <- fib(n - 1)
        b <- fib(n - 2)
      } yield a + b

    assert(fib(0).value == 1)
    assert(fib(1).value == 1)
    assert(fib(2).value == 2)
    assert(fib(3).value == 3)
    assert(fib(4).value == 5)
    assert(fib(5).value == 8)
  }

  final case class Lst[+A](value: Need[(A, Lst[A])]) {
    def map[B](f: A => B): Lst[B] = Lst(value.map { case (h, t) => (f(h), t.map(f)) })
    def take(n: Int): Need[List[A]] =
      if (n <= 0) Need.now(Nil)
      else value.flatMap { case (h, t) =>
        t.take(n - 1).map(h :: _)
      }
  }

  val l123: Lst[Int] = Lst(Need((1, l123.map(_ + 1))))
  property("lst") {
    assert(l123.take(3).value == List(1, 2, 3))
    assert(l123.take(1024).value == (1 to 1024).toList)
    assert(l123.take(1024 * 1024).value == (1 to (1024 * 1024)).toList)
  }
}
