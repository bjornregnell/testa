import testa.*

@main 
def runTests(): Unit = 

  testCase("testa testa"){
    val t1 = testCase("a")(1/2)
    val t2 = testCase("a")(1/0)
    t1.shouldEqual(0)
    TestCase.tests.size
  }.shouldEqual(2)

  TestCase.report()
