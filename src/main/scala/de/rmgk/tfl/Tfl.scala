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
  case class Text(str: String) extends Value
  implicit def stringToText(str: String): Text = Text(str)


  type Environment = Map[Identifier, Value]

  case class Closure(fun: Fun, environment: Environment) extends Value {
    override def format(): String = s"Closure(${fun.format()},$environment)"
    override def tex(): String = s"Closure(${fun.tex()},$environment)"
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
    case _: Closure | _ : Text => Set()
  }


  def interpret(term: Term, environment: Environment): Value = {
    term match {
      case id: Identifier                  => environment(id)
      case fun: Fun                        =>
        val environmentOfFreeVariables = environment -- (environment.keySet -- freeVariables(fun))
        Closure(fun, environmentOfFreeVariables)
      case App(functionTerm, argumentTerm) =>
        val argument = interpret(argumentTerm, environment)
        interpret(functionTerm, environment) match {
          case Closure(function, closureEnvironment) =>
            interpret(function.body, closureEnvironment + (function.parameter -> argument))
          case t @ Text(s1) =>
            argument match {
              case Text(s2) => Text(s1 + s2)
              case _ => Closure('x =>: 'x(t), environment)
            }

        }
      case v: Value                        => v
    }
  }


  def subs(term: Term, environment: Environment): Term = {
    term match {
      case id: Identifier                  => environment.getOrElse(id, id)
      case Fun(parameter, body)            =>
        Fun(parameter, substitute(body, environment))
      case App(functionTerm, argumentTerm) =>
        App(substitute(functionTerm, environment), substitute(argumentTerm, environment))
      case Closure(fun, env)               =>
        val senv = env.map { case (k, v) => k -> interpret(substitute(v, env - k), Map()) }
        substitute(fun, senv)
      case t: Text                         => t
    }
  }

  def substitute(term: Term, environment: Environment): Term = {
    subs(subs(term, environment), Map())
  }

  case class Configuration(term: Term, environment: Environment) {
    def derive(name: String, to: Configuration, premises: List[Rule] = Nil): Some[Rule] = Some(Rule(name, this, to, premises))
    def format(): String = substitute(term, environment).format()
    def tex(): String = substitute(term, environment).tex()
  }
  case class Rule(name: String, from: Configuration, to: Configuration, premises: List[Rule] = Nil) {
    def tex(): String =
      s"\\infer{${premises.map(_.tex).mkString("{", "} \\and {", "}")}}{${from.tex} → ${to.tex}} \\named{$name}"
    def format(): String =
      s"${to.format} [$nameTree]"
    def nameTree(): String = s"$name [${premises.map(_.nameTree()).mkString(",")}]"
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
        println(p.format)
        stepAll(p.to)
    }
  }

  def toInt(value: Term): Option[Int] = {


    def evaluateToText(t: Term): Text = t match {
      case txt: Text => txt
      case _         =>
        evaluateToText(interpret(t(""), Map()))
    }
    val res = evaluateToText(value("1"))
    res match {
      case Text(str) => Some(str.count(_ == '1'))
      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
//    val program = ('x =>: 'x('x))('y =>: 'y)
    val program = add(two)(two)("1")("0")("0")
//    pprintln(toInt(program))
//    val res = interpret(program, Map())
//    pprintln(res)
//    pprintln(substitute(substitute(res, Map()), Map()))
//    println(program.tex())
//    pprintln(interpret(program, Map()))
    pprintln(program)
    println(interpret(program, Map()).format)
    pprintln(stepAll(Configuration(program, Map())))
  }

}
