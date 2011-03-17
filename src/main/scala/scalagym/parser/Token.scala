package scalagym.parser

sealed trait Token
case class PartialToken(value: String) extends Token

sealed trait Value extends Token
case class NotNullValue(value: String) extends Value
case object NullValue extends Value // null

sealed trait Operator extends Token
sealed trait UnaryOperator extends Operator
sealed trait DuaryOperator extends Operator
sealed trait MultipleOperator extends Operator
case object Equal extends UnaryOperator // eq
case object NotEqual extends UnaryOperator // ne
case object Like extends UnaryOperator // lk
case object GreaterThan extends UnaryOperator // gt
case object GreaterThanOrEqual extends UnaryOperator // ge
case object LesserThan extends UnaryOperator // lt
case object LesserThanOrEqual extends UnaryOperator // le
case object Between extends DuaryOperator // bw
case object In extends MultipleOperator // in

sealed trait Logical extends Token
sealed trait UnaryLogical extends Logical
sealed trait DuaryLogical extends Logical
case object Not extends UnaryLogical
case object And extends DuaryLogical
case object Or extends DuaryLogical

case class Expression(exp: String) extends Token

object TokenBuilder {
  implicit def stringToToken(s: String) = new {
    def toToken: Token = s match {
      case "null" => NullValue
      case "eq" => Equal
      case "ne" => NotEqual
      case "lk" => Like
      case "gt" => GreaterThan
      case "ge" => GreaterThanOrEqual
      case "lt" => LesserThan
      case "le" => LesserThanOrEqual
      case "bw" => Between
      case "in" => In
      case "not" => Not
      case "and" => And
      case "or" => Or
      case _ => throw new UnknownTokenException()
    }
  }
}

case class UnknownTokenException() extends Exception
