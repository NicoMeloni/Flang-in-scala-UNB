package br.unb.cic.flang

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SimpleParserTest extends AnyFlatSpec with Matchers {
  val parser = new SimpleParser

  "The parser" should "parse integer constants" in {
    val result = parser.parseAll(parser.cint, "42")
    result.successful should be (true)
    result.get should be (CInt(42))
  }

  it should "parse boolean constants (true)" in {
    val result = parser.parseAll(parser.cbool, "true")
    result.successful should be (true)
    result.get should be (CBool(1))
  }

  it should "parse boolean constants (false)" in {
    val result = parser.parseAll(parser.cbool, "false")
    result.successful should be (true)
    result.get should be (CBool(0))
  }

  it should "parse addition expressions" in {
    val result = parser.parseAll(parser.add, "1 + 2")
    result.successful should be (true)
    result.get should be (Add(CInt(1), CInt(2)))
  }

  it should "parse multiplication expressions" in {
    val result = parser.parseAll(parser.mul, "3 * 4")
    result.successful should be (true)
    result.get should be (Mul(CInt(3), CInt(4)))
  }

  it should "parse function application expressions" in {
    val result = parser.parseAll(parser.app, "f(5)")
    result.successful should be (true)
    result.get should be (App("f", CInt(5)))
  }

  it should "parse if-then-else expressions" in {
    val input = "if (true) { 1 } else { 0 }"
    val result = parser.parseAll(parser.ifThenElse, input)
    result.successful should be (true)
    result.get should be (IfThenElse(CBool(1), CInt(1), CInt(0)))
  }

  it should "parse variable identifiers" in {
    val result = parser.parseAll(parser.id, "x")
    result.successful should be (true)
    result.get should be (Id("x"))
  }

  it should "parse a function declaration" in {
    val input = "fun double(x) { x + x }"
    val result = parser.parseAll(parser.fdecl, input)
    result.successful should be (true)
    result.get should be (FDeclaration("double", "x", Add(Id("x"), Id("x"))))
  }

  it should "parse a complete program" in {
    val input = 
      """fun double(x) { x + x }
        |
        |if (true) {
        |  double(10)
        |} else {
        |  0
        |}""".stripMargin

    val result = parser.parseAll(parser.program, input)
    
    result.successful should be (true)
    
    val (funcs, exprs) = result.get

    funcs should contain (FDeclaration("double", "x", Add(Id("x"), Id("x"))))
    exprs should contain (IfThenElse(CBool(1), App("double", CInt(10)), CInt(0)))
  }
}