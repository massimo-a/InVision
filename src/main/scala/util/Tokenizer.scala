package raytracing.util

sealed trait Token
final case class IntToken(value: String, next: Token) extends Token
final case class DecimalToken(value: String, next: Token) extends Token
final case class KeywordToken(value: String, next: Token) extends Token
final case class StringToken(value: String, next: Token) extends Token
final case class OpToken(value: String, next: Token) extends Token
final case class ErrToken(value: String, message: String, next: Token) extends Token
final case class BracketToken(value: String, bracketType: Int, next: Token) extends Token
final case class CommentToken(value: String, comment: String, next: Token) extends Token
final case class EOF() extends Token

object Token {
	private case class StringIterator(position: Int, string: String) {
		def current(): String = {
			string(position).toString();
		}
		def isEnd(): Boolean = {
			position >= string.length
		}
		def isKeyword(): Boolean = {
			"abcdefghijklmnopqrstuvwxyz_,.?!;:".contains(current.toLowerCase)
		}
		def isNumber(): Boolean = {
			"0123456789.".contains(current)
		}
		def isString(): Boolean = {
			"\"\'".contains(current)
		}
		def isWhiteSpace(): Boolean = {
			" \t\n\r".contains(current)
		}
		def isOp(): Boolean = {
			"-+*/=%^".contains(current)
		}
		def isLeftBracket(): Boolean = {
			"({[".contains(current)
		}
		def isRightBracket(): Boolean = {
			"]})".contains(current)
		}
		def commentStart(): Boolean = {
			"#".contains(current)
		}
		private def readWhile(cond: StringIterator => Boolean, accu: String): String = {
			if(!isEnd && cond(this)) {
				return StringIterator(position+1, string).readWhile(cond, accu+current())
			} else return accu
		}
		def readWhile(cond: StringIterator => Boolean): String = {
			readWhile(cond, "");
		}
	}
	private def reverse(curr: Token, accu: Token): Token = {
		return curr match {
			case EOF() => accu
			case IntToken(v: String, next: Token) => reverse(next, IntToken(v, accu))
			case DecimalToken(v: String, next: Token) => reverse(next, DecimalToken(v, accu))
			case KeywordToken(v: String, next: Token) => reverse(next, KeywordToken(v, accu))
			case StringToken(v: String, next: Token) => reverse(next, StringToken(v, accu))
			case OpToken(v: String, next: Token) => reverse(next, OpToken(v, accu))
			case BracketToken(v: String, t: Int, next: Token) => reverse(next, BracketToken(v, t, accu))
			case CommentToken(v: String, c: String, next: Token) => reverse(next, CommentToken(v, c, accu))
			case ErrToken(v: String, msg: String, next: Token) => reverse(next, ErrToken(v, msg, accu))
		}
	}
	def reverse(curr: Token): Token = {
		reverse(curr, EOF())
	}
	private def tokenize(stream: StringIterator, accu: Token): Token = {
		val pos = stream.position;
		val str = stream.string
		if(stream.isEnd) {
			return reverse(accu)
		} else if(stream.isWhiteSpace) {
			return tokenize(StringIterator(pos+1, str), accu)
		} else if(stream.isKeyword) {
			val keyword = stream.readWhile(_.isKeyword)
			return tokenize(StringIterator(pos+keyword.length, str), KeywordToken(keyword, accu))
		} else if(stream.isNumber) {
			val num = stream.readWhile(_.isNumber)
			return (num.count(_ == '.') match {
				case 0 => tokenize(StringIterator(pos+num.length, str), IntToken(num, accu))
				case 1 => tokenize(StringIterator(pos+num.length, str), DecimalToken(num, accu))
				case _ => tokenize(StringIterator(pos+num.length, str), ErrToken(num, "Multiple decimal points in number", accu))
			})
		} else if(stream.isString) {
			val word = StringIterator(pos+1, str).readWhile(s => !s.isString)
			return tokenize(StringIterator(pos+word.length+2, str), StringToken(word, accu))
		} else if(stream.isOp) {
			val op = stream.readWhile(_.isOp)
			return tokenize(StringIterator(pos+op.length, str), OpToken(op, accu))
		} else if(stream.isLeftBracket || stream.isRightBracket) {
			if(stream.isLeftBracket) {
				return tokenize(StringIterator(pos+1, str), BracketToken(stream.current, 0, accu))
			} else {
				return tokenize(StringIterator(pos+1, str), BracketToken(stream.current, 1, accu))
			}
		} else if(stream.commentStart) {
			val comment = StringIterator(pos+1, str).readWhile(s => !"#".contains(s.current))
			return tokenize(StringIterator(pos+comment.length+2, str), CommentToken("#", comment, accu))
		} else {
			return tokenize(StringIterator(pos+1, str), ErrToken(stream.current(), "Unrecognized symbol", accu))
		}
	}
	def tokenize(str: String): Token = {
		tokenize(StringIterator(0, str), EOF())
	}
	def toString(tok: Token): String = {
		return tok match {
			case EOF() => ""
			case IntToken(v: String, next: Token) => "('" + v + "', Integer) \n" + toString(next)
			case DecimalToken(v: String, next: Token) => "('" + v + "', Decimal) \n" + toString(next)
			case KeywordToken(v: String, next: Token) => "('" + v + "', Keyword) \n" + toString(next)
			case StringToken(v: String, next: Token) => "('" + v + "', String) \n" + toString(next)
			case OpToken(v: String, next: Token) => "('" + v + "', Operation) \n" + toString(next)
			case BracketToken(v: String, t: Int, next: Token) => {
				if(t == 0) "('" + v + "', Left Bracket) \n" + toString(next)
				else "('" + v + "', Right Bracket) \n" + toString(next)
			}
			case CommentToken(v: String, c: String, next: Token) => "('" + v + "', Comment : '" + c + "') \n" + toString(next)
			case ErrToken(v: String, msg: String, next: Token) => "('" + v + "', Error : '" + msg + "') \n" + toString(next)
		}
	}
	def tokenizeFile(filename: String): Token = {
		import scala.io.Source
		import java.io.FileNotFoundException
		
		return try {
			tokenize(Source.fromFile(filename).getLines.mkString)
		} catch {
			case x: FileNotFoundException => {
				ErrToken(filename, "File was not found. Is the filename spelled correctly? Are you pointing me to the right file path?", EOF())
			}
		}
	}
}

object TokenMain {
	def main(args: Array[String]): Unit = {
		println(Token.toString(Token.tokenizeFile("C:\\Users\\Massimo Due\\Documents\\GitHub\\TRAC3R\\src\\main\\scala\\util\\testTokenizer.txt")))
		println(Token.toString(Token.tokenizeFile("testTokenizer.txt")))
	}
}