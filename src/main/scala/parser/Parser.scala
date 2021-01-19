package parser

import parser.tokenizer.{NumberToken, ObjectToken, Token}

case class Parser() {
  def parse(tokens: List[Token]): List[(String, Map[String, List[String]])] = {
    var res = List[(String, Map[String, List[String]])]()
    var curr = tokens
    while(curr.nonEmpty) {
      curr.head match {
        case ObjectToken(v: String) =>
          val parsed = parseObject(curr.drop(2))
          res = res.prepended((v, parsed._2))
          curr = parsed._1
        case _ =>
          null
      }
    }
    res
  }

  @scala.annotation.tailrec
  private def parseObject(tokens: List[Token], accu: Map[String, List[String]] = Map()): (List[Token], Map[String, List[String]]) = {
    if(tokens.head.value == "}") {
      (tokens.drop(1), accu)
    } else {
      val prop = tokens.head.value
      val a = parseValue(tokens.drop(2))
      parseObject(a._2, accu + (prop -> a._1))
    }
  }

  private def parseValue(tokens: List[Token]): (List[String], List[Token]) = {
    if(tokens.head.value == "(") {
      readVector(tokens.drop(1))
    } else {
      (List(tokens.head.value), tokens.drop(2))
    }
  }

  @scala.annotation.tailrec
  private def readVector(tokens: List[Token], accu: List[String] = List()): (List[String], List[Token]) = {
    if(tokens.head.value == ";") {
      (accu.reverse, tokens.drop(1))
    } else {
      tokens.head match {
        case NumberToken(v: String) =>
          readVector(tokens.drop(1), v :: accu)
        case _ =>
          readVector(tokens.drop(1), accu)
      }
    }
  }
}
