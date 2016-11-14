package regexp.api2

import scala.util.Try

sealed trait Token
object Token {
  case object Seq extends Token
  case class Sym(c: Char) extends Token
  case object Ast extends Token
  case object Bar extends Token
  case object L extends Token
  case object R extends Token
  case object Pls extends Token
  case object Que extends Token
  case class Bnd(n: Int, m: Int) extends Token
  case class Cls(s: List[Char], f: Char => Boolean) extends Token
  case object Dot extends Token
}

object Parser {

  val not = (!_): Boolean => Boolean

  def token(c: Char): Token = {
    import Token._
    c match {
      case '*' => Ast
      case '|' => Bar
      case '(' => L
      case ')' => R
      case '?' => Que
      case '+' => Pls
      case '.' => Dot
      case c   => Sym(c)
    }
  }

  def scan(s: List[Char]): List[Token] = {
    insertSeqs(process(s))
  }

  def insertSeqs(ts: List[Token]): List[Token] = {
    import Token._
    ts match {
      case Nil      => ts
      case t :: Nil => ts
      case a :: (ts @ (b :: _)) => {
        if (lseq(a) && rseq(b)) {
          a :: Seq :: insertSeqs(ts)
        } else {
          a :: insertSeqs(ts)
        }
      }
    }
  }

  def lseq(t: Token): Boolean = {
    import Token._
    t match {
      case Bar => false
      case L   => false
      case _   => true
    }
  }

  def rseq(t: Token): Boolean = {
    import Token._
    t match {
      case _: Sym => true
      case L      => true
      case _: Cls => true
      case Dot    => true
      case _      => false
    }
  }

  def reads(xs: List[Char]): Option[(Int, List[Char])] = {

    def toInt(cs: List[Char]): Option[Int] = {
      Try(cs.toString.toInt).toOption
    }

    def loop(num: List[Char], xs: List[Char]): (List[Char], List[Char]) = {
      xs match {
        case c :: cs => {
          if (isSpace(c)) {
            loop(num, cs)
          } else {
            if (isDigit(c)) {
              loop(c :: num, cs)
            } else {
              (num, cs)
            }
          }
        }
        case _ => (num, xs)
      }
    }

    val (num, tail) = loop(Nil, xs)
    num match {
      case Nil => None
      case _   => toInt(num).map(_ -> tail)
    }
  }

  def process(xs: List[Char]): List[Token] = {
    import Token._
    xs match {
      case Nil             => Nil
      case '\\' :: c :: cs => Cls(List('\\', c), symClassPred(c)) :: process(cs)
      case '{' :: cs => {
        reads(cs) match {
          case Some((n, '}' :: s1)) => Bnd(n, n) :: process(s1)
          case Some((n, ',' :: s1)) => {
            reads(s1) match {
              case Some((m, '}' :: s2)) => Bnd(n, m) :: process(s2)
              case _                    => token('{') :: process(cs)
            }
          }
          case _ => token('{') :: process(cs)
        }
      }
      case '[' :: '^' :: cs => {
        val (s, p, xs) = processCls(cs)
        Cls('[' :: '^' :: s, p.andThen(not)) :: process(xs)
      }
      case '[' :: cs => {
        val (s, p, xs) = processCls(cs)
        Cls('[' :: s, p) :: process(xs)
      }
      case c :: cs => token(c) :: process(cs)
    }
  }

  def processCls(xs: List[Char]): (List[Char], Char => Boolean, List[Char]) = {

    xs match {
      case Nil => {
        parseError(Nil)
      }
      case ']' :: cs => {
        (']' :: Nil, { (_: Char) => false }, cs)
      }
      case '\\' :: c :: cs if isSymClassChar(c) => {
        val (s, p, xs) = processCls(cs)
        ('\\' :: c :: s, { (x: Char) => symClassPred(c)(x) || p(x) }, xs)
      }
      case '\\' :: c :: cs => {
        val (s, p, xs) = processCls(cs)
        ('\\' :: c :: s, { (x: Char) => x == c || p(x) }, xs)
      }
      case c :: '-' :: e :: cs if e != ']' => {
        val (s, p, xs) = processCls(cs)
        (c :: '-' :: e :: s, { (d: Char) => (c <= d && d <= e) || p(d) }, xs)
      }
      case c :: cs => {
        val (s, p, xs) = processCls(cs)
        (c :: s, { (b: Char) => b == c || p(b) }, xs)
      }
    }
  }

  def isSymClassChar(c: Char): Boolean = {
    c match {
      case 'w' => true
      case 'W' => true
      case 'd' => true
      case 'D' => true
      case 's' => true
      case 'S' => true
      case _   => false
    }
  }

  def symClassPred(c: Char): Char => Boolean = {

    c match {
      case 'w' => isWordChar _
      case 'd' => isDigit _
      case 's' => isSpace _
      case 'W' => not.compose(isWordChar _)
      case 'D' => not.compose(isDigit _)
      case 'S' => not.compose(isSpace _)
      case c   => ((c == _): Char => Boolean)
    }
  }

  def isWordChar(c: Char): Boolean = {
    c == '_' || isAlphaNum(c)
  }

  def isAlphaNum(c: Char): Boolean = {
    isDigit(c) || isAlpha(c)
  }

  def isAlpha(c: Char): Boolean = {
    c match {
      case 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm' => true
      case 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z' => true
      case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M' => true
      case 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' => true
      case _ => false
    }
  }

  def isDigit(c: Char): Boolean = {
    c match {
      case '0' => true
      case '1' => true
      case '2' => true
      case '3' => true
      case '4' => true
      case '5' => true
      case '6' => true
      case '7' => true
      case '8' => true
      case '9' => true
      case _   => false
    }
  }

  def isSpace(c: Char): Boolean = {
    c match {
      case '\t' => true
      case '\n' => true
      case '\r' => true
      case '\f' => true
      case _    => false
    }
  }

  def parseError[A](ts: List[Token]): A = {
    throw new IllegalArgumentException("cannot parse regular expression")
  }
}
