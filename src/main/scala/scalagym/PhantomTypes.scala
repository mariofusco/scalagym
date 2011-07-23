package scalagym

object BuilderPattern {
  sealed trait Preparation
  case object Neat extends Preparation
  case object OnTheRocks extends Preparation
  case object WithWater extends Preparation

  sealed trait Glass
  case object Short extends Glass
  case object Tall extends Glass
  case object Tulip extends Glass

  case class OrderOfScotch(val brand:String
                           , val mode:Preparation
                           , val isDouble:Boolean
                           , val glass:Option[Glass])

  trait TRUE
  trait FALSE

  class ScotchBuilder[HB, HM, HD](val theBrand:Option[String]
                                  , val theMode:Option[Preparation]
                                  , val theDoubleStatus:Option[Boolean]
                                  , val theGlass:Option[Glass]) {
    def withBrand(b:String) =
      new ScotchBuilder[TRUE, HM, HD](Some(b), theMode, theDoubleStatus, theGlass)

    def withMode(p:Preparation) =
      new ScotchBuilder[HB, TRUE, HD](theBrand, Some(p), theDoubleStatus, theGlass)

    def isDouble(b:Boolean) =
      new ScotchBuilder[HB, HM, TRUE](theBrand, theMode, Some(b), theGlass)

    def withGlass(g:Glass) = new ScotchBuilder[HB, HM, HD](theBrand, theMode, theDoubleStatus, Some(g))
  }

  implicit def enableBuild(builder:ScotchBuilder[TRUE, TRUE, TRUE]) = new {
    def build() =
      new OrderOfScotch(builder.theBrand.get, builder.theMode.get, builder.theDoubleStatus.get, builder.theGlass);
  }

  def builder = new ScotchBuilder[FALSE, FALSE, FALSE](None, None, None, None)
}

object Scotch extends App {
  import BuilderPattern._

  val scotch = builder.withBrand("Lagavulin").withGlass(Short).isDouble(false).withMode(Neat).build
  println(scotch)
}