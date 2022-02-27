object Main extends App:
  import Selector.*

  case class Test(
    test1: Test1
  )

  case class Test1(
    test2: Test2
  )

  case class Test2(
    value: Int
  )

  println(select[Test](_.test1.test2.value))
end Main