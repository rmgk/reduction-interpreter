package de.rmgk.tfl

import pprint.pprintln

import scala.language.implicitConversions

object Tfl {
  sealed trait Term {
    def apply(argument: Term) = App(this, argument)
    def =>:(identifier: Identifier): Fun = Fun(identifier, this)
    def format(): String = toString
    def tex(): String = toString
  }
  sealed trait Value extends Term

  case class Identifier(scala: Symbol) extends Term {
    override def format(): String = scala.name
    override def tex(): String = scala.name
  }
  implicit def symbolToId(symbol: Symbol): Identifier = Identifier(symbol)
  case class Fun(parameter: Identifier, body: Term) extends Term {
    override def format(): String = s"(${parameter.format} => ${body.format})"
    override def tex(): String = s"(\\fun{${parameter.tex}}{${body.tex()}})"
  }
  case class App(function: Term, argument: Term) extends Term {
    override def format(): String = s"(${function.format} ${argument.format})"
    override def tex(): String = s"(${function.tex}\\ ${argument.tex})"
  }

  type Environment = Map[Identifier, Value]

  case class Closure(fun: Fun, environment: Environment) extends Value {
    override def format(): String = fun.format()
    override def tex(): String = fun.tex()
  }

  def id: Fun = 'x =>: 'x
  def zero: Fun = 'f =>: 'x =>: 'x
  def one: Fun = id
  def succ: Fun = 'a =>: 'f =>: 'x =>: 'f('a('f)('x))
  def add: Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a('f)('b('f)('x))
  def two: App = add(one)(one)
  def times: Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a('b('f)('x))

  def freeVariables(term: Term): Set[Identifier] = term match {
    case id: Identifier => Set(id)
    case Fun(parameter, body) => freeVariables(body) - parameter
    case App(function, argument) => freeVariables(function) ++ freeVariables(argument)
    case _: Closure => Set()
  }


  def interpret(term: Term, environment: Environment): Value = {
    term match {
      case id: Identifier                  => environment(id)
      case fun: Fun                        =>
        val environmentOfFreeVariables = environment -- (environment.keySet -- freeVariables(fun))
        Closure(fun, environmentOfFreeVariables)
      case App(functionTerm, argumentTerm) =>
        val argument = interpret(argumentTerm, environment)
        val Closure(function, closureEnvironment) = interpret(functionTerm, environment)
        interpret(function.body, closureEnvironment + (function.parameter -> argument))
      case v: Value                        => v
    }
  }

  case class Configuration(term: Term, environment: Environment) {
    def derive(name: String, to: Configuration, premises: List[Rule] = Nil): Some[Rule] = Some(Rule(name, this, to, premises))
    def format(): String = term.format()
    def tex(): String = s"${term.tex()} | ..."
  }
  case class Rule(name: String, from: Configuration, to: Configuration, premises: List[Rule] = Nil) {
    def tex(): String =
      s"\\infer{${premises.map(_.tex).mkString("{", "} \\and {", "}")}}{${from.tex} → ${to.tex}} \\named{$name}"
  }

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
    step(conf) match {
      case None => conf
      case Some(p) =>
        println(p.tex)
        stepAll(p.to)
    }
  }

  def toInt(value: Term): Int = {


    val res = interpret(value(succ)(zero), Map())
    pprintln(res)
    0
  }

  def main(args: Array[String]): Unit = {
//    val program = 'f =>: 'x =>: 'x //'x =>: 'x ('x)('y =>: 'y)
    val p = two(two)
    pprintln(toInt(p))
//    println(program.tex())
//    pprintln(interpret(program, Map()))
//    stepAll(Configuration(program, Map()))
  }

}
