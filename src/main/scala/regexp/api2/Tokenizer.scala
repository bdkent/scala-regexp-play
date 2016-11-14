package regexp.api2

import regexp.api.Semiring

// LOL
object Tokenizer {
  def toRegex(tokens: List[Token])(implicit semi: Semiring[Boolean]): RegW[Boolean, Char] = {

    def loop(in: List[Either[Token, RegW[Boolean, Char]]]): RegW[Boolean, Char] = {

      in match {
        case Nil => {
          Regex.eps()
        }
        case Left(s: Token.Sym) :: xs => {
          loop(Right(Regex.char(s.c)) :: xs)
        }
        case Right(r: RegW[Boolean, Char]) :: Left(Token.Ast) :: xs => {
          loop(Right(Regex.rep(r)) :: xs)
        }
        case Right(r1: RegW[Boolean, Char]) :: Left(Token.Seq) :: Right(r2: RegW[Boolean, Char]) :: xs => {
          Regex.seq_(r1, loop(Right(r2) :: xs))
        }
        case Right(r1: RegW[Boolean, Char]) :: Left(Token.Bar) :: Right(r2: RegW[Boolean, Char]) :: xs => {
          Regex.alt(r1, loop(Right(r2) :: xs))
        }
        case Left(Token.L) :: Right(r: RegW[Boolean, Char]) :: Left(Token.R) :: xs => {
          loop(Right(r) :: xs)
        }
        case Right(r: RegW[Boolean, Char]) :: Left(Token.Pls) :: xs => {
          loop(Right(Regex.rep1(r)) :: xs)
        }
        case Right(r: RegW[Boolean, Char]) :: Left(Token.Que) :: xs => {
          loop(Right(Regex.opt(r)) :: xs)
        }
        case Right(r: RegW[Boolean, Char]) :: Left(b: Token.Bnd) :: xs => {
          loop(Right(Regex.brep(b.n, b.m, r)) :: xs)
        }
        case Left(c: Token.Cls) :: xs => {
          loop(Right(Regex.psym(c.s, c.f)) :: xs)
        }
        case Left(Token.Dot) :: xs => {
          loop(Right(Regex.anySym()) :: xs)
        }
        case Right(r1: RegW[Boolean, Char]) :: Left(Token.Seq) :: xs => {
          Regex.seq_(r1, loop(xs))
        }
        case x => {
          println(x)
          ???
        }
      }
    }

    loop(tokens.map(Left(_)))
  }
}
