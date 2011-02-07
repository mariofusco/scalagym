package scalagym

/**
 * @author Mario Fusco
 */

object Comprehension extends Application {

  val persons = List(
    new Person("Domenico", 71) addChild new Person("Mario", 36) addChild new Person("Irma", 32),
    new Person("Michele", 63) addChild new Person("Cesidio", 30) addChild new Person("Daniela", 26),
    new Person("Pietro", 71) addChild new Person("Antonio", 41)
  )

//  val result = persons flatMap (_.children) filter (_.age > 35) map (_.name)
//  val result = persons flatMap (p => p.children) filter (c => c.age > 35) map (c => c.name)
  val result = persons.map(p => p.children)
          .foldRight(List[Person]()) {(xs, ys) => xs ::: ys}
          .filter (c => c.age > 35) map (c => c.name)
  println(result)

  val resultFor = for {
    p <- persons
    c <- p.children if (c.age > 35)
  } yield c.name
  println(resultFor)
}

class Person(n: String, a: Int) {
  val name = n
  val age = a
  var children: List[Person] = Nil
  def addChild(child: Person) = { children = child :: children; this }
  override def toString() = name
}