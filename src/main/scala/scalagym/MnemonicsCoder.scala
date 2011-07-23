package scalagym

class MnemonicsCoder(words: List[String]) {

  private val mnemonics = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                              '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  /** Invert the mnemonics map to give a map from chars 'A' ... 'Z' to '2' ... '9' */
  private val charCode: Map[Char, Char] =
    for ((digit, str) <- mnemonics; letter <- str) yield (letter -> digit)

  /** Maps a word to the digit string it can represent */
  private def wordCode(word: String): String =
    word.toUpperCase map charCode

  /** A map from digit strings to the words that represent them,
   *  e,g. “5282” ‐> Set(“Java”, “Kata”, “Lava”, ...) */
  private val wordsForNum: Map[String, List[String]] =
    (words groupBy wordCode) withDefaultValue List.empty

  /** Return all ways to encode a number as a list of words */
  private def encode(number: String): Set[List[String]] =
    if (number.isEmpty)
      Set(List())
    else {
      for {
        splitPoint <- 1 to number.length
        word <- wordsForNum(number take splitPoint)
        rest <- encode(number drop splitPoint)
      } yield word :: rest
    }.toSet

  /** Maps a number to a list of all word phrases that can represent it */
  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")
}

object Tester extends App {
  val words = List("dog", "cat", "mouse", "horse", "zebra", "rhino", "cow", "bull", "pig", "snake", "frog", "eagle"
    , "ant", "bird", "koala", "deer", "fly", "yak", "lion", "bear", "fox", "ape", "bee", "worm", "dove", "snail", "bug")

  val coder = new MnemonicsCoder(words)

  println(coder.translate("269546622893272369364273"))
}