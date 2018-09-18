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
  case class Fun(parameter: Identifier, body: Term) extends Term
  case class App(function: Term, argument: Term) extends Term

  type Environment = Map[Identifier, Value]

  case class Closure(fun: Fun, environment: Environment) extends Value



  def interpret(term: Term, environment: Environment): Value = {
    term match {
      case id: Identifier                  => environment(id)
      case fun: Fun                        => Closure(fun, environment)
      case App(functionTerm, argumentTerm) =>
        val Closure(function, closureEnvironment) = interpret(functionTerm, environment)
        val argument = interpret(argumentTerm, environment)
        interpret(function.body, closureEnvironment + (function.parameter -> argument))
      case closure: Closure                => closure
    }
  }

  case class Configuration(term: Term, environment: Environment) {
    def derive(name: String, to: Configuration, premises: List[Rule] = Nil): Some[Rule] = Some(Rule(name, this, to, premises))
  }
  case class Rule(name: String, from: Configuration, to: Configuration, premises: List[Rule] = Nil)

  def step(conf: Configuration): Option[Rule] = {
    val environment = conf.environment
    conf.term match {
      case _: Value                        => None
      case id: Identifier                  => conf.derive("env", conf.copy(term = environment(id)))
      case fun: Fun => conf.derive("close", conf.copy(Closure(fun, environment)))
      case App(Closure(function, closureEnvironment), argument: Value) => conf.derive("app", {
        Configuration(function.body, closureEnvironment + (function.parameter -> argument))
      })
      case app @ App(closure: Value, argument) =>
        step(conf.copy(argument)).flatMap{ inner =>
          conf.derive("context", inner.to.copy(app.copy(argument = inner.to.term)), List(inner))
        }
      case app @ App(function, _) =>
        step(conf.copy(function)).flatMap { inner =>
          conf.derive("context", inner.to.copy(app.copy(inner.to.term)), List(inner))
        }
    }
  }

  def stepAll(conf: Configuration): Configuration = {
    pprintln(conf)
    step(conf) match {
      case None => conf
      case Some(p) =>
        pprintln(p.name)
        stepAll(p.to)
    }
  }

  def main(args: Array[String]): Unit = {
    val program = 'x ↦ 'x ('x) apply ('y ↦ 'y)
    pprintln(program)
    pprintln(interpret(program, Map()))
    pprintln(step(Configuration(program, Map())))
    println("====")
    stepAll(Configuration(program, Map()))
  }

}
