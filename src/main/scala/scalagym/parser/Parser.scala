package scalagym.parser

import TokenBuilder._

trait Parser { this: ExpressionBuilder =>

  def getQuery(s: String) = new {
    def withFieldName(fieldName: String) = buildExpression(tokenize(s)) withFieldName fieldName
  }

  private def tokenize(s: String): List[Token] =
    parseWords(s.split(" ").toList, List[Token](), false).reverse

  private def parseWords(words: List[String], tokens: List[Token], inValue: Boolean): List[Token] = words match {
    case Nil => tokens
    case w :: ws => parseWord(w, inValue) match {
      case PartialToken(s) => parseWords(s + " " + ws.head :: ws.tail, tokens, true)
      case t => parseWords(ws, t :: tokens, false)
    }
  }

  private val StartAndEndWithQuotes = """^"(.+)"$""".r
  private val StartWithQuotes = """^"(.+)$""".r
  private val EndWithQuotes = """^(.+)"$""".r

  private def parseWord(word: String, inValue: Boolean): Token = word match {
    case StartAndEndWithQuotes(value) => if (inValue) throw ParseException() else NotNullValue(value)
    case StartWithQuotes(value) => if (inValue) throw ParseException() else PartialToken(value)
    case EndWithQuotes(value) => if (inValue) NotNullValue(value) else throw ParseException()
    case s => if (inValue) PartialToken(s) else s.toToken
  }

  private def buildExpression(tokens: List[Token]): Expression =
    ((List[Token]() /: tokens)(foldFunction)).head.asInstanceOf[Expression]

  def foldFunction(ts: List[Token], t: Token): List[Token] = t match {
    case pt: PartialToken => throw ParseException()
    case v: Value =>  v :: ts
    case op: UnaryOperator => unaryOperation(op, ts.head.asInstanceOf[Value]) :: ts.tail
    case op: DuaryOperator => ts match {
      case t1 :: t2 :: tt => duaryOperation(op, t2.asInstanceOf[NotNullValue], t1.asInstanceOf[NotNullValue]) :: tt
      case _ => throw ParseException()
    }
    case op: MultipleOperator => {
      val values = ts.takeWhile(_.isInstanceOf[NotNullValue]).reverse.asInstanceOf[List[NotNullValue]]
      multipleOperation(op, values) :: ts.drop(values.size)
    }
    case logic: UnaryLogical => unaryLogical(logic, ts.head.asInstanceOf[Expression]) :: ts.tail
    case logic: DuaryLogical => ts match {
      case t1 :: t2 :: tt => duaryLogical(logic, t2.asInstanceOf[Expression], t1.asInstanceOf[Expression]) :: tt
      case _ => throw ParseException()
    }
    case e: Expression => throw ParseException()
  }
}

case class ParseException() extends Exception