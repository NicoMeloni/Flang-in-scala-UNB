package br.unb.cic.flang

sealed trait Expr

case class CInt(v: Integer) extends Expr
case class Add(lhs: Expr, rhs: Expr) extends Expr
case class Mul(lhs: Expr, rhs: Expr) extends Expr
case class Id(name: String) extends Expr
case class App(name: String, arg: Expr) extends Expr
case class FDecl(name: String, arg: String, body: Expr) extends Expr
//////////////Nicolas
case class IfThenElse(cond: Expr, thenExpr: Expr, elseExpr: Expr) extends Expr
case class CBool(v: Integer) extends Expr
object CBoolValidator {
  def apply(v: Integer): Either[String,Boolean] = Int.unbox(v) match {
    case 0 => Right(false)
    case 1 => Right(true)
    case _ => Left(s"Valor inválido: $v. É esperado 0 ou 1.")
  }
}
////////////// 