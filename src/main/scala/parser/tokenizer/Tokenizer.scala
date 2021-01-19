package parser.tokenizer

import scala.util.matching.Regex

final case class Tokenizer() {
  val numbers = "0123456789.-"
  val numberCheck = new Regex("^[-+]?[0-9]\\d*(\\.\\d+)?$")
  val letters = new Regex("[a-zA-Z]")
  val operators = "(){}:;,"

  @scala.annotation.tailrec
  def tokenize(str: String, curr: Int = 0, accu: List[Token] = List()): List[Token] = {
    if(curr >= str.length) {
      accu.reverse
    } else {
      if(str(curr) == '#') {
        val nextWord = readWhile(x => letters.matches(x), str, curr + 1)
        tokenize(str: String, nextWord._2, accu.prepended(ObjectToken(nextWord._1.toUpperCase())))
      } else if(numbers.contains(str(curr))) {
        val nextWord = readWhile(x => numbers.contains(x), str, curr)
        if(!numberCheck.matches(nextWord._1)) {
          throw new Exception(s"Incorrectly formatted number: ${nextWord._1}")
        }
        tokenize(str: String, nextWord._2, accu.prepended(NumberToken(nextWord._1)))
      } else if(letters.matches(str(curr).toString)) {
        val nextWord = readWhile(x => letters.matches(x), str, curr)
        tokenize(str: String, nextWord._2, accu.prepended(PropertyToken(nextWord._1.toUpperCase())))
      } else if(operators.contains(str(curr))) {
        tokenize(str: String, curr + 1, accu.prepended(OperatorToken(str(curr).toString)))
      } else {
        tokenize(str: String, curr + 1, accu)
      }
    }
  }

  @scala.annotation.tailrec
  private def readWhile(f: String => Boolean, str: String, curr: Int, word: String = ""): (String, Int) = {
    if(curr >= str.length || !f(str(curr).toString)) {
      (word, curr)
    } else {
      readWhile(f, str, curr + 1, word + str(curr))
    }
  }
}
