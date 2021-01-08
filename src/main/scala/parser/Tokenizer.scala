package parser

final case class Token(value: String, tokenType: Int)

case object Tokenizer {
  val WhiteSpace = 0
  val Keyword = 1
  val Number = 2
  val Operator = 3
  val SpecialChar = 4
  val Unknown = 5

  private val letters = "abcdefghijklmnopqrstuvwxyz"
  private val special = "!@#$%^&()_[]{}\\|,.?;:'\"`~"
  private val numbers = "0123456789"
  private val operators = "+-*/=<>"
  private val whiteSpace = "\t\r\n "

  @scala.annotation.tailrec
  def tokenize(str: String, curr: Int = 0, accu: List[Token] = List()): List[Token] = {
    if(curr >= str.length) {
      accu.reverse
    } else {
      val nextWord = if(letters.contains(str(curr).toLower)) {
        val w = readWhile(x => letters.contains(x.toLower) && x != ' ', str, curr)
        (w._1, Keyword, w._2)
      } else if(numbers.contains(str(curr))) {
        val w = readWhile(x => numbers.contains(x) && x != ' ', str, curr)
        (w._1, Number, w._2)
      } else if(special.contains(str(curr))) {
        val w = readWhile(x => special.contains(x) && x != ' ', str, curr)
        (w._1, SpecialChar, w._2)
      } else if(whiteSpace.contains(str(curr))) {
        val w = readWhile(x => x == ' ', str, curr)
        (w._1, WhiteSpace, w._2)
      } else if(operators.contains(str(curr))) {
        val w = readWhile(x => operators.contains(x), str, curr)
        (w._1, Operator, w._2)
      } else {
        val w = readWhile(x => x != ' ', str, curr)
        (w._1, Unknown, w._2)
      }
      tokenize(str, nextWord._3, accu.prepended(Token(nextWord._1, nextWord._2)))
    }
  }

  @scala.annotation.tailrec
  def readWhile(f: Char => Boolean, str: String, curr: Int = 0, word: String = ""): (String, Int) = {
    if(curr >= str.length || !f(str(curr))) {
      (word, curr)
    } else {
      readWhile(f, str, curr + 1, word + str(curr))
    }
  }

  def parseToScene(): Unit = {

  }

  def main(args: Array[String]): Unit = {
    println(
      tokenize("ADD SPHERE 100 300 500 500 COLOR 255 255 255").filter(t => t.tokenType != WhiteSpace)
    )
  }
}
