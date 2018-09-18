package de.rmgk.tfl

import pprint.pprintln

import scala.language.implicitConversions

object Tfl {
  sealed trait Term {
    def apply(argument: Term) = App(this, argument)
  }
  sealed trait Value extends Term

  case class Identifier(scala: Symbol) extends Term {
    def ↦(body: Term): Fun = Fun(this, body)
  }
  implicit def symbolToId(symbol: Symbol): Identifier = Identifier(symbol)
  case class Fun(parameter: Identifier, body: Term) extends Value
  case class App(function: Term, argument: Term) extends Term

  type Environment = Map[Identifier, Value]

  case class Closure(fun: Fun, environment: Environment) extends Value


  def interpret(term: Term, environment: Environment): Value = {
    term match {
      case id : Identifier => environment(id)
      case fun: Fun => Closure(fun, environment)
      case App(functionTerm, argumentTerm) =>
        val Closure(function, closureEnvironment) = interpret(functionTerm, environment)
        val argument = interpret(argumentTerm, environment)
        interpret(function.body, closureEnvironment + (function.parameter -> argument))
      case closure: Closure => closure
    }
  }

  def main(args: Array[String]): Unit = {
    val program = 'x ↦ 'x('x) apply ('y ↦ 'y)
    pprintln(interpret(program, Map()))
  }

}
