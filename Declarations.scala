package br.unb.cic.flang

import MErrState._
import cats.data.EitherT
import cats.data.State
import cats.MonadError
// import cats.syntax.applicative._       // for pure
// import cats.syntax.applicativeError._  // for raiseError

case class FDeclaration(name: String, arg: String, body: Expr)

object Declarations {
  def lookup(
      name: String,
      declarations: List[FDeclaration]
  ): MStateEH[FDeclaration] = declarations match {
    case List() => EitherT.leftT(s"Function $name is not declared")
    case (f@FDeclaration(n, a, b))::_ if n == name => pure(f)
    case _::fs => lookup(name, fs)  
  }

}
