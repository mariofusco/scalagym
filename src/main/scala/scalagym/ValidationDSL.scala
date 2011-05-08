package scalagym

/**
 * @author Mario Fusco - twitter: @mariofusco
 */
object ValidationDSL {

  implicit def toValidated[A](a: A) = new {
    def validable(): Validated[A] = Success(a)
  }

  def check[A, B](a: A, b: B)(f: A => Boolean): Validated[A] =
    if (f(a)) Success(a)
    else Failure(a, List(b))

  implicit def toValidationFunc[B](b: B) = new {
    def validIf[A](f: A => Boolean)(a: A): Validated[A] =
      check(a, b)(f)
  }

  def validate[A](a: A) = new {
    def using(l: (A => Validated[A])*) = (a.validable /: l)(_ flatMap _)
  }
}

object PersonValidator extends Application {

  import ValidationDSL._

  case class Person(name: String, age: Int)

  def validName(p: Person): Validated[Person] =
    check(p, "Name must start with a capital letter") {
      _.name.headOption.exists(_.isUpper)
    }

  def validAge(p: Person): Validated[Person] =
    check(p, "Age must be in range") {
      0 to 130 contains _.age
    }

  // Validation style 1: flatMap on Validated monad
  val p1 = Person("Mario", 37).validable flatMap validAge flatMap validName

  // Validation style 2: for-comprehension
  val p2 = for {
    x <- Person("mario", 137).validable
    y <- validAge(x)
    z <- validName(y)
  } yield z

  // Validation style 3: DSL
  val p3 = validate(Person("mario", 137)) using (
    "Age must be in range" validIf {
      0 to 130 contains _.age
    },
    "Name must start with a capital letter" validIf {
      _.name.headOption.exists(_.isUpper)
    }
  )

  println(p1)
  println(p2)
  println(p3)
}

sealed trait Validated[A] {
  def map[B](f: A => B): Validated[B]
  def flatMap[B](f: A => Validated[B]): Validated[B]
}

case class Success[A](a: A) extends Validated[A] {
  def map[B](f: A => B): Validated[B] = Success(f(a))

  def flatMap[B](f: A => Validated[B]): Validated[B] = f(a)
}

case class Failure[A, C](a: A, l: List[C]) extends Validated[A] {

  def this(a: A, c: C) = this(a, List(c))

  def map[B](f: A => B): Validated[B] = Failure(f(a), l)

  def flatMap[B](f: A => Validated[B]): Validated[B] = {
    f(a) match {
      case Success(b) => Failure[B, C](b, l)
      case Failure(b, l1) => Failure[B, C](b, l1.asInstanceOf[List[C]] ::: l)
    }
  }
}