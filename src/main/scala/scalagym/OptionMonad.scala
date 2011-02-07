package scalagym

/**
 * @author Mario Fusco
 */

object OptionMonad extends Application {

  def firstName = Some("Mario")
  def lastName = Some("Fusco")

  val fullName = for {
    fname <- firstName; lname <- lastName
  } yield fname + lname

  println(fullName)

  val fullName2 = firstName flatMap (fname => lastName flatMap (lname => Some(fname + lname)))
  println(fullName2)
}