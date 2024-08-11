package br.unb.cic.flang

import MErrState._

import Declarations._
import Substitution._
import cats.data.{EitherT, State}


object Interpreter {
  def eval(expr: Expr, declarations: List[FDeclaration]): MStateEH[Integer] = expr match {
    case CInt(v) => pure(v)
    ///////////
    case CBool(v) => CBoolValidator(v) match {
      case Right(b) => pure(if (b) 1 else 0)
      case Left(err) => EitherT.leftT(err)
    }
    ///////////
    case Add(lhs, rhs) => for {
      l <- eval(lhs, declarations)
      r <- eval(rhs, declarations)    
    } yield l + r
    
    case Mul(lhs, rhs) => for {
      l <- eval(lhs, declarations)
      r <- eval(rhs, declarations)  
    } yield l * r

    case Id(name) => for {
      state <- EitherT.right(State.get[S])
      res <- lookupVar(name, state)
    } yield res

    case App(name, arg) => for {
      fdecl <- lookup(name, declarations)
      value <- eval(arg, declarations) 
      s1 <- EitherT.right(State.get[S])
      s2 <- EitherT.right(State.set(declareVar(fdecl.arg, value, s1)))
      res <- eval(fdecl.body, declarations)
    } yield res
    
    ///////////Nicolas
    case IfThenElse(cond, thenExpr, elseExpr) => for {
      condition <- eval(cond, declarations)
      res <- if (condition != 0) eval(thenExpr, declarations) else eval(elseExpr, declarations)
    } yield res
    ///////////
  }
}
