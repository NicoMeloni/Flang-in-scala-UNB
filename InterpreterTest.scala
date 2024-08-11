package br.unb.cic.flang

import org.scalatest._
import flatspec._
import matchers._

import Interpreter._
import Declarations._
import MErrState._

class InterpreterTest extends AnyFlatSpec with should.Matchers {
  
  val inc = FDeclaration("inc", "x", Add(Id("x"), CInt(1)))
  val bug = FDeclaration("bug", "x", Add(Id("y"), CInt(1)))

  val declarations = List(inc, bug)
  val initialState: S = List()

  "eval CInt(5)" should "return an integer value 5." in {
    val c5 = CInt(5)
    val state = eval(c5, declarations)
    val (_, res) = state.value.run(initialState).value
    res should be (Right(5))
  }

  "eval Add(CInt(5), CInt(10)) " should "return an integer value 15." in {
    val c5  = CInt(5)
    val c10 = CInt(10)
    val add = Add(c5, c10)
    val state = eval(add, declarations) 
    val (_, res) = state.value.run(initialState).value
    res should be (Right(15))
  }

  "eval Add(CInt(5), Add(CInt(5), CInt(10))) " should "return an integer value 20." in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val add = Add(c5, Add(c5, c10))
    val state = eval(add, declarations)
    val (_, res) = state.value.run(initialState).value
    res should be (Right(20))
  }

  "eval Mul(CInt(5), CInt(10))" should "return an integer value 50" in {
    val c5 = CInt(5)
    val c10 = CInt(10)
    val mul = Mul(c5, CInt(10))
    val state = eval(mul, declarations) 
    val (_, res) = state.value.run(initialState).value
    res should be (Right(50))
  }

  "eval App(inc, 99) " should "return an integer value 100" in {
    val app = App("inc", CInt(99))
    val state = eval(app, declarations) 
    val (_, res) = state.value.run(initialState).value
    res should be (Right(100))
  }

  "eval App(foo, 10) " should "raise an error." in {
    val app = App("foo", CInt(10))
    val state = eval(app, declarations)
    val (_, res) = state.value.run(initialState).value
    val types = res.isLeft
    types should be (true)
  }

  "eval Add(5, App(bug, 10)) " should "raise an error." in {
    val c5  = CInt(5)
    val app = App("bug", CInt(10))
    val add = Add(app, c5)
    val state = eval(add, declarations)
    val (_, res) = state.value.run(initialState).value
    val types = res.isLeft
    types should be (true)
  }
/////////////////////////////////////////////////////Nicolas
  "eval CBool(0)" should "return an integer value 0." in {
  val cbool0 = CBool(0)
  val state = eval(cbool0, declarations)
  val (_, res) = state.value.run(initialState).value
  res should be (Right(0))
}

"eval CBool(1)" should "return an integer value 1." in {
  val cbool1 = CBool(1)
  val state = eval(cbool1, declarations)
  val (_, res) = state.value.run(initialState).value
  res should be (Right(1))
}

"eval CBool(2)" should "raise an error." in {
  val cbool2 = CBool(2)
  val state = eval(cbool2, declarations)
  val (_, res) = state.value.run(initialState).value
  res should be (Left("Valor inválido: 2. É esperado 0 ou 1."))
}

"eval IfThenElse(CBool(1), CInt(10), CInt(20))" should "return an integer value 10." in {
    val ifExpr = IfThenElse(CBool(1), CInt(10), CInt(20))
    val state = eval(ifExpr, declarations)
    val (_, res) = state.value.run(initialState).value
    res should be (Right(10))
  }

  "eval IfThenElse(CBool(0), CInt(10), CInt(20))" should "return an integer value 20." in {
    val ifExpr = IfThenElse(CBool(0), CInt(10), CInt(20))
    val state = eval(ifExpr, declarations)
    val (_, res) = state.value.run(initialState).value
    res should be (Right(20))
  }

  "eval IfThenElse(CBool(2), CInt(10), CInt(20))" should "raise an error." in {
    val ifExpr = IfThenElse(CBool(2), CInt(10), CInt(20))
    val state = eval(ifExpr, declarations)
    val (_, res) = state.value.run(initialState).value
    res should be (Left("Valor inválido: 2. É esperado 0 ou 1."))
  }

/////////////////////////////////////////////////////
}
