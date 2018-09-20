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
  sealed trait Result extends Term
  sealed trait Value extends Result

  case class Identifier(scala: Symbol) extends Term {
    override def format(): String = scala.name
    override def tex(): String = scala.name
  }
  implicit def symbolToId(symbol: Symbol): Identifier = Identifier(symbol)

  case class Fun(parameter: Identifier, body: Term) extends Value {
    override def format(): String = s"(${parameter.format} => ${body.format})"
    override def tex(): String = s"(\\fun{${parameter.tex}}{${body.tex()}})"
  }
  case class App(function: Term, argument: Term) extends Term {
    override def format(): String = s"(${function.format} ${argument.format})"
    override def tex(): String = s"(${function.tex}\\ ${argument.tex})"
  }
  case class Text(str: String) extends Value
  implicit def stringToText(str: String): Text = Text(str)

  case class Error(msg: String) extends Result
  case class TryCatch(body: Term, handler: Term) extends Term


  type Environment = Map[Identifier, Value]

//  case class Closure(fun: Fun, environment: Environment) extends Value {
//    override def format(): String = s"Closure(${fun.format()},$environment)"
//    override def tex(): String = s"Closure(${fun.tex()},$environment)"
//  }

  def id: Fun = 'x =>: 'x
  def zero: Fun = 'f =>: 'x =>: 'x
  def one: Fun = id
  def succ: Fun = 'a =>: 'f =>: 'x =>: 'f ('a ('f)('x))
  def add: Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a ('f)('b ('f)('x))
  def two: App = add(one)(one)
  def times: Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a ('b ('f)('x))

  def freeVariables(term: Term): Set[Identifier] = term match {
    case id: Identifier          => Set(id)
    case Fun(parameter, body)    => freeVariables(body) - parameter
    case App(function, argument) => freeVariables(function) ++ freeVariables(argument)
    case TryCatch(body, handler) => freeVariables(body) ++ freeVariables(handler)
    case _: Text | _: Error      => Set()
  }


  def interpret(term: Term, environment: Environment): Result = {
    term match {
      case id: Identifier => environment(id)
//      case fun: Fun                        =>
//        val environmentOfFreeVariables = environment -- (environment.keySet -- freeVariables(fun))
//        Closure(fun, environmentOfFreeVariables)
      case App(functionTerm, argumentTerm) =>
        val argument = interpret(argumentTerm, environment)
        interpret(functionTerm, environment) match {
          case function: Fun =>
            interpret(subs(function.body, function.parameter, argument), Map())
          case t@Text(s1)    =>
            argument match {
              case Text(s2) => Text(s1 + s2)
              case _        => 'x =>: t(argument('x))
            }
          case other: Error  => other

        }
      case TryCatch(body, handler)         => interpret(body, environment) match {
        case _: Error => interpret(handler, environment)
        case other    => other
      }
      case v: Result                       => v
    }
  }

  def subs(term: Term, parameter: Identifier, argument: Term): Term = {
    val freeVar = freeVariables(argument)
    assert(freeVar == Set(), s"unbound identifier $freeVar")
    term match {
      case id: Identifier if id == parameter      => argument
      case Fun(param, body) if param != parameter =>
        Fun(param, subs(body, parameter, argument))
      case App(functionTerm, argumentTerm)        =>
        App(subs(functionTerm, parameter, argument), subs(argumentTerm, parameter, argument))
      case other                                  => other
    }
  }

  case class Configuration(term: Term) {
    def derive(name: String, to: Term, premises: List[Rule] = Nil): Some[Rule] =
      Some(Rule(name, this, this.copy(to), premises))
    def format(): String = term.format()
    def tex(): String = term.tex()
  }
  case class Rule(name: String, from: Configuration, to: Configuration, premises: List[Rule] = Nil) {
    def tex(): String =
      s"\\infer{${premises.map(_.tex).mkString("{", "} \\and {", "}")}}{${from.tex}\\\\ → ${to.tex}} \\named{$name}"
    def format(): String =
      s"${to.format} [$nameTree]"
    def nameTree(): String = s"$name [${premises.map(_.nameTree()).mkString(",")}]"
  }

  def step(conf: Configuration): Option[Rule] = {

    def context(term: Term, outer: Term => Term): Option[Rule] = {
      term match {
        case err: Error => conf.derive("error", err)
        case nonError   =>
          step(conf.copy(nonError)).flatMap { inner =>
          conf.derive("context", outer(inner.to.term), List(inner))
        }
      }

    }

    conf.term match {
      case _: Result      => None
      case id: Identifier => None

      case App(function: Fun, argument: Value) => conf.derive("app",
                                                              subs(function.body, function.parameter, argument))
      case App(Text(s1), Text(s2))             => conf.derive("concat text", Text(s1 + s2))
      case App(t@Text(s1), v: Fun)             => conf.derive("concat other", 'x =>: t(v('x)))

      case TryCatch(Error(_), handler) => conf.derive("catch", handler)
      case TryCatch(value: Value, _)   => conf.derive("try", value)

      case tc@TryCatch(body, _)        => context(body, inner => tc.copy(body = inner))
      case app@App(_: Value, argument) => context(argument, inner => app.copy(argument = inner))
      case app@App(function, _)        => context(function, inner => app.copy(function = inner))

    }
  }

  @scala.annotation.tailrec
  def stepAll(conf: Configuration): Configuration = {
    step(conf) match {
      case None    => conf
      case Some(p) =>
        println(p.format())
        //pprintln(p, height = 500)
        stepAll(p.to)
    }
  }

  def toInt(value: Term): Option[Int] = {

    @scala.annotation.tailrec
    def evaluateToText(t: Term): Text = t match {
      case txt: Text => txt
      case _         =>
        evaluateToText(interpret(t(""), Map()))
    }

    val res = evaluateToText(value("1"))
    res match {
      case Text(str) => Some(str.count(_ == '1'))
      case _         => None
    }
  }

  def main(args: Array[String]): Unit = {
    val program = TryCatch(one(Error("oh noes")), two)
//    val program = times(add(one)(two))(one(add(succ(two))(times(two)(succ(two)))))
    pprintln(toInt(program))
//    val res = interpret(program, Map())
//    pprintln(res)
//    pprintln(substitute(substitute(res, Map()), Map()))
//    println(program.tex())
//    pprintln(interpret(program, Map()))
    println(program.tex)
//    println(interpret(program("1")("0"), Map()).format)
    pprintln(stepAll(Configuration(program)))
  }

}
