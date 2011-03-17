package scalagym.parser

trait ExpressionBuilder {
  val placeholder = "!!!"
  implicit def toFieldNameSetter(e: Expression) = new {
    def withFieldName(fieldName: String): String = e.exp.replaceAll(placeholder, fieldName)
  }

  def unaryOperation(op: UnaryOperator, value: Value): Expression
  def duaryOperation(op: DuaryOperator, v1: NotNullValue, v2: NotNullValue): Expression
  def multipleOperation(op: MultipleOperator, values: List[NotNullValue]): Expression
  def unaryLogical(op: UnaryLogical, exp: Expression): Expression
  def duaryLogical(op: DuaryLogical, e1: Expression, e2: Expression): Expression
}

trait JPAExpressionBuilder extends ExpressionBuilder { this: ValueRenderer =>

  def unaryOperation(op: UnaryOperator, value: Value): Expression = op match {
    case Equal => value match {
      case NullValue => Expression(placeholder + " is " + render(value))
      case v: NotNullValue => Expression(placeholder + " = " + render(value))
    }
    case NotEqual => value match {
      case NullValue => Expression(placeholder + " is not " + render(value))
      case v: NotNullValue => Expression(placeholder + " != " + render(value))
    }
    case Like => unOpOnNonNullable(value, "like")
    case GreaterThan => unOpOnNonNullable(value, ">")
    case GreaterThanOrEqual => unOpOnNonNullable(value, ">=")
    case LesserThan => unOpOnNonNullable(value, "<")
    case LesserThanOrEqual => unOpOnNonNullable(value, "<=")
  }

  private def unOpOnNonNullable(value: Value, op: String): Expression = value match {
    case NullValue => throw ParseException()
    case v: NotNullValue => Expression(placeholder + " " + op + " " + render(value))
  }

  def duaryOperation(op: DuaryOperator, v1: NotNullValue, v2: NotNullValue): Expression = op match {
    case Between => Expression(placeholder + " >= " + render(v1) + " and " + placeholder + " <= " + render(v2))
  }

  def multipleOperation(op: MultipleOperator, values: List[NotNullValue]): Expression = op match {
    case In => Expression(placeholder + " in (" + values.map(render).mkString(", ") + ")")
  }

  def unaryLogical(op: UnaryLogical, exp: Expression): Expression = op match {
    case Not => Expression("not (" + exp.exp + ")")
  }

  def duaryLogical(op: DuaryLogical, e1: Expression, e2: Expression) = op match {
    case Or => Expression("(" + e1.exp + ") or (" + e2.exp + ")")
    case And => Expression("(" + e1.exp + ") and (" + e2.exp + ")")
  }
}

trait ValueRenderer {
  def render(v: Value): String
}

trait StringValueRenderer extends ValueRenderer {
  def render(v: Value): String = v match {
    case NullValue => "null"
    case NotNullValue(value) => "\"" + value + "\""
  }
}

trait IntValueRenderer extends ValueRenderer {
  def render(v: Value): String = v match {
    case NullValue => "null"
    case NotNullValue(value) => value
  }
}

trait DateValueRenderer extends ValueRenderer {
  def render(v: Value): String = v match {
    case NullValue => "null"
    case NotNullValue(value) => "'" + value + "'"
  }
}