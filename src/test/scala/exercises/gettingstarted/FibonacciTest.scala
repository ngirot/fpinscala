package exercises.gettingstarted

import org.scalatest.FunSuite

class FibonacciTest extends FunSuite {

  test("The 0th Fibonacci number is 0") {
    assert(Fibonacci.fib(0) == 0)
  }

  test("The 1st Fibonacci number is 1") {
    assert(Fibonacci.fib(1) == 1)
  }

  test("The 3rd Fibonacci number is 2") {
    assert(Fibonacci.fib(3) == 2)
  }

  test("The 24rd Fibonacci number is 46368") {
    assert(Fibonacci.fib(24) == 46368)
  }

}
