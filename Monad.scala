package br.unb.cic.flang

import cats.MonadError
import cats.instances.either._
import cats.data.EitherT
import cats.data.State


object MErrState {
  type S = List[(String, Integer)]
  type MError[A] = Either[String, A]
  type MState[A] = State[S,A]
  type MStateEH[A] = EitherT[MState,String,A]
  
  def pure[A](a: A): MStateEH[A] = EitherT.pure(a) 

  def declareVar(name: String, value: Integer, state: S): S =
    (name, value) :: state

  def lookupVar(name: String, state: S): MStateEH[Integer] = state match {
    case List()                      => EitherT.leftT(s"Variable $name is not declared")
    case (n, v) :: tail if n == name => EitherT.rightT(v)
    case _ :: tail                   => lookupVar(name, tail)
  }

  def assertError[A](m: MError[A]) : Boolean = m match {
    case Left(_) => true
    case Right(_) => false  
  }
}
