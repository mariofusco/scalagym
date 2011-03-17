package scalagym.parser

object Tester extends Application {
  val q1 = """null eq not"""
  val q2 = """"5" lt "7" "9" "11" in or"""
  val q3 = """"20110131T000000" ge "20100201T000000" "20100331T000000" bw or"""
  val q4 = """"M%" lk "Marco Pancotti" eq not and "Pino%" lk or"""

  println(new Parser with JPAExpressionBuilder with StringValueRenderer getQuery q1 withFieldName "a")
  println(new Parser with JPAExpressionBuilder with IntValueRenderer getQuery q2 withFieldName "b")
  println(new Parser with JPAExpressionBuilder with DateValueRenderer getQuery q3 withFieldName "c")
  println(new Parser with JPAExpressionBuilder with StringValueRenderer getQuery q4 withFieldName "d")
}