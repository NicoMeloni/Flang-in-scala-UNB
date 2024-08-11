package br.unb.cic.flang

import CBoolValidator._
import Declarations._
import scala.util.parsing.combinator._

class SimpleParser extends JavaTokenParsers {

  def program: Parser[(List[FDeclaration], List[Expr])] = 
  (functions ~ exprs) ^^ {
    case funcs ~ exprs => (funcs, exprs)
  }

  def exprs: Parser[List[Expr]] = rep1(expr)

  def functions: Parser[List[FDeclaration]] = rep(fdecl)

  def expr: Parser[Expr] = 
    ifThenElse | app | add 

  def fdecl: Parser[FDeclaration] = 
    ("fun" ~ ident ~ "(" ~ ident ~ ")" ~ "{" ~ expr ~ "}") ^^ {
      case "fun" ~ name ~ "(" ~ arg ~ ")" ~ "{" ~ body ~ "}" => 
        FDeclaration(name, arg, body)
    }

  def cint: Parser[CInt] = 
    wholeNumber ^^ { num => CInt(num.toInt) }

  def cbool: Parser[CBool] = 
    ("true" | "false") ^^ {
      case "true"  => CBool(1)
      case "false" => CBool(0)
    }

  def term: Parser[Expr] = 
    cint | cbool | id | "(" ~> expr <~ ")"
  
  def add: Parser[Expr] = chainl1(mul, "+" ^^^ Add)
    //(expr ~ "+" ~ expr) ^^ {case lhs ~ "+" ~ rhs => Add(lhs, rhs)}

  def mul: Parser[Expr] = chainl1(term, "*" ^^^ Mul)
    //(expr ~ "*" ~ expr) ^^ {case lhs ~ "*" ~ rhs => Mul(lhs, rhs)}

  def app: Parser[App] = 
    (ident ~ "(" ~ expr ~ ")") ^^ {case name ~ "(" ~ arg ~ ")" => App(name, arg)}

  def ifThenElse: Parser[IfThenElse] = 
    ("if" ~ "(" ~ expr ~ ")" ~ "{" ~ expr ~ "}" ~ "else" ~ "{" ~ expr ~ "}") ^^ {
      case "if" ~ "(" ~ cond ~ ")" ~ "{" ~ thenExpr ~ "}" ~ "else" ~ "{" ~ elseExpr ~ "}" => 
        IfThenElse(cond, thenExpr, elseExpr)
    }

  def id: Parser[Id] = 
    ident ^^ { name => Id(name) }      

}
