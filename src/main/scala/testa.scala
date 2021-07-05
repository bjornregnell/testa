package testa

import scala.util.{Try, Success, Failure}

case class TestCase[A](name: String, thunk: () => A):
  def run(): A = thunk()

  def shouldEqual[A](value: => A): Unit =
    val test = TestCase(name, () => 
      val result = thunk() 
      if result == value then value 
      else throw Exception(s"$result should equal $value")
    )
    TestCase.add(test)

  def shouldThrow(e: Exception): Unit = 
    def classOf(a: Any): String = a.getClass.getName
    val test = TestCase(name, () =>
      Try(thunk()) match
      case Failure(e2) if classOf(e2) == classOf(e) => e
      case _ => throw Exception(s"should throw ${classOf(e)}")
    )
    TestCase.add(test)

object TestCase:
  var tests = Vector.empty[TestCase[?]] 

  def add[A](t: TestCase[A]): Unit = tests = (tests :+ t)
  
  def runAll(): Seq[String] = 
    for i <- tests.indices yield 
      val t0 = System.nanoTime
      def showTimeMillis = ((System.nanoTime - t0)/1_000_000.0).toString
      val result = Try(tests(i).run()) match
        case Success(v) => Console.GREEN + "SUCCESS " + Console.RESET + s"result $v"
        case Failure(e) => Console.RED + "FAILURE " + Console.RESET + e.getMessage
      s"$i. ${tests(i).name} $showTimeMillis ms $result"
    end for
  
  def report(): Unit = runAll().foreach(println) 
end TestCase

def testCase[A](name: String)(a: => A) = TestCase(name, () => a)