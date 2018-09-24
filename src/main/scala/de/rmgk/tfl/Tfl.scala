package de.rmgk.tfl

import java.util.UUID

import pprint.pprintln

import scala.language.implicitConversions

object Tfl {
  sealed trait Term {
    def apply(argument: Term) = App(this, argument)
    def =>:(identifier: Identifier): Fun = Fun(identifier, this)
    def tex(): String = toString
  }
  sealed trait Stuck extends Term
  sealed trait Value extends Stuck

  case class Identifier(scala: Symbol) extends Term {
    override def toString(): String = scala.name
    override def tex(): String = scala.name
  }
  implicit def symbolToId(symbol: Symbol): Identifier = Identifier(symbol)

  case class Fun(parameter: Identifier, body: Term) extends Value {
    override def toString(): String = s"(${parameter.toString} => ${body.toString})"
    override def tex(): String = s"(\\fun{${parameter.tex}}{${body.tex()}})"
  }
  case class App(function: Term, argument: Term) extends Term {
    override def toString(): String = s"(${function.toString} ${argument.toString})"
    override def tex(): String = s"(${function.tex}\\ ${argument.tex})"
  }
  case class Text(str: String) extends Value
  implicit def stringToText(str: String): Text = Text(str)

  case class Error(msg: String) extends Stuck
  case class TryCatch(body: Term, handler: Term) extends Term

  case class Reactive(id: UUID) extends Value
  case class Var(init: Term) extends Term {
    override def toString(): String = s"Var(${init.toString()})"
  }
  case class Access(target: Term) extends Term {
    override def toString(): String = s"${target.toString()}.value"
  }

  def Let(identifier: Identifier, boundValue: Term, body: Term): App = (identifier =>: body) apply boundValue
  def execute(terms: Term*): Term = terms.reduceRight { (t, acc) => Let('_, t, acc) }

  type Time = Int

  case class Stored(value     : Stuck,
                    time      : Time,
                    operator  : Term,
                    inputs    : Set[Reactive],
                    outputs   : Set[Reactive],
                    continuous: Boolean)
  object Store {
    def Evt(value: Stuck) = Stored(value, 0, unit, Set.empty, Set.empty, continuous = false)
  }

  case class Store(mapping: Map[Reactive, Stored]) {
    def apply(r: Reactive): Stored = mapping.apply(r)
    def update(r: Reactive, v: Stuck): Store = copy(mapping.updated(r, mapping(r).copy(value = v)))
    def value(r: Reactive): Stuck = mapping.get(r).map(_.value).getOrElse(Error(s"unknown reactive $r"))
    def add(r: Reactive, stored: Stored): Store = {
      assert(!mapping.contains(r), s"duplicate add $r")
      copy(mapping.updated(r, stored))
    }
  }

//  case class Closure(fun: Fun, environment: Environment) extends Value {
//    override def format(): String = s"Closure(${fun.format()},$environment)"
//    override def tex(): String = s"Closure(${fun.tex()},$environment)"
//  }

  def freeVariables(term: Term): Set[Identifier] = term match {
    case id: Identifier          => Set(id)
    case Fun(parameter, body)    => freeVariables(body) - parameter
    case App(function, argument) => freeVariables(function) ++ freeVariables(argument)
    case TryCatch(body, handler) => freeVariables(body) ++ freeVariables(handler)
    case Var(init)               => freeVariables(init)
    case Access(target)          => freeVariables(target)
    case _: Text |
         _: Error |
         _: Reactive             => Set()
  }

  case class InterpreterResult(value: Stuck, store: Store)

  def interpret(interpretedTerm: Term, store: Store): InterpreterResult = {
    interpretedTerm match {
      case id: Identifier => InterpreterResult(Error(s"unbound identifier $id"), store)

      case App(functionTerm, argumentTerm) =>
        val evaluatedFunction = interpret(functionTerm, store)
        val argument = interpret(argumentTerm, evaluatedFunction.store)
        evaluatedFunction.value match {
          case function: Fun =>
            interpret(subs(function.body, function.parameter, argument.value), argument.store)
          case t@Text(s1)    =>
            argument.value match {
              case Text(s2) => InterpreterResult(Text(s1 + s2), argument.store)
              case _        => InterpreterResult('x =>: t(argument.value('x)), argument.store)
            }
          case other: Error  => evaluatedFunction.copy(other)
          case r: Reactive   =>
            InterpreterResult(r, argument.store(r) = argument.value)

        }

      case TryCatch(body, handler) =>
        val res = interpret(body, store)
        res.value match {
          case _: Error => interpret(handler, res.store)
          case other    => res.copy(other)
        }

      case Var(init) =>
        val res = interpret(init, store)
        val r = Reactive(UUID.randomUUID())
        InterpreterResult(r, store.add(r, Store.Evt(res.value)))

      case Access(target) =>
        val res = interpret(target, store)
        res.value match {
          case r: Reactive => InterpreterResult(store.value(r), res.store)
          case other       => res.copy(other)
        }


      case v: Stuck => InterpreterResult(v, store)
    }
  }

  def subs(term: Term, parameter: Identifier, argument: Term): Term = {
    val freeVar = freeVariables(argument)
    assert(freeVar == Set(), s"unbound identifier $freeVar")

    def subsi(t: Term) = subs(t, parameter, argument)

    term match {
      case id: Identifier                  => if (id == parameter) argument else id
      case fun@Fun(param, body)            =>
        if (param == parameter) fun else
                                      Fun(param, subsi(body))
      case App(functionTerm, argumentTerm) =>
        App(subs(functionTerm, parameter, argument), subsi(argumentTerm))
      case Access(target)                  => Access(subsi(target))
      case Var(init)                       => Var(subsi(init))
      case TryCatch(body, handler)         => TryCatch(subsi(body), subsi(handler))
      case _: Text |
           _: Error |
           _: Reactive |
           _: Identifier                   => term
    }
  }

  case class Configuration(term: Term, store: Store) {
    def derive(name: String, to: Term, premises: List[Rule] = Nil, newStore: Store = store): Some[Rule] =
      Some(Rule(name, this, Configuration(to, newStore), premises))
    def format(): String = term.toString()
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
            inner.to.derive("context", outer(inner.to.term), List(inner))
          }
      }

    }

    conf.term match {
      case _: Stuck       => None
      case id: Identifier => None

      case App(function: Fun, argument: Value) => conf.derive("app",
                                                              subs(function.body, function.parameter, argument))
      case App(Text(s1), Text(s2))             => conf.derive("concat text", Text(s1 + s2))
      case App(t@Text(s1), v: Fun)             => conf.derive("concat other", 'x =>: t(v('x)))

      case TryCatch(Error(_), handler) => conf.derive("catch", handler)
      case TryCatch(value: Value, _)   => conf.derive("try", value)

      case App(r: Reactive, v: Value) => conf.derive("fire", r, newStore = conf.store(r) = v)
      case Access(target: Reactive)   => conf.derive("access", conf.store.value(target))
      case Var(init: Value)           =>
        val r = Reactive(UUID.randomUUID())
        conf.derive("var", r, newStore = conf.store.add(r, Store.Evt(init)))


      case tc@TryCatch(body, _)        => context(body, inner => tc.copy(body = inner))
      case app@App(_: Value, argument) => context(argument, inner => app.copy(argument = inner))
      case app@App(function, _)        => context(function, inner => app.copy(function = inner))
      case acc@Access(target)          => context(target, inner => acc.copy(target = inner))
      case v@Var(init)                 => context(init, inner => v.copy(init = inner))

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

  def toInt(value: Term): Either[Term, Int] = {

    @scala.annotation.tailrec
    def evaluateFun(t: Term, store: Store): Term = {
      val res = interpret(t, store)
      res.value match {
        case t: Fun =>
          evaluateFun(t(""), res.store)
        case other  => other
      }
    }

    val res = evaluateFun(value("1"), Store(Map()))
    res match {
      case Text(str) => Right(str.count(_ == '1'))
      case other     => Left(other)
    }
  }


  val id   : Fun = 'x =>: 'x
  val zero : Fun = 'f =>: 'x =>: 'x
  val unit : Fun = zero
  val one  : Fun = id
  val succ : Fun = 'a =>: 'f =>: 'x =>: 'f ('a ('f)('x))
  val add  : Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a ('f)('b ('f)('x))
  val two  : App = add(one)(one)
  val times: Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a ('b ('f)('x))
  val four       = times(two)(two)
  val ten        = times(succ(four))(two)

  def account: Term =
    Let('balance, Var(one),
        Let('deposit, 'amount =>: 'balance (add(Access('balance))('amount)),
            execute(
              'deposit (one),
              'deposit (two),
              'deposit (ten),
              Access('balance)
            )
        )
    )


  def main(args: Array[String]): Unit = {
    val program = account
//    val program = times(add(one)(two))(one(add(succ(two))(times(two)(succ(two)))))
    println(program.toString())
    val res = stepAll(Configuration(ten, Store(Map())))
    pprintln(toInt(res.term))
    pprintln(toInt(program))
//    val res = interpret(program, Map())
//    pprintln(res)
//    pprintln(substitute(substitute(res, Map()), Map()))
//    println(program.tex())
//    pprintln(interpret(program, Map()))
    println(program.tex)
//    println(interpret(program("1")("0"), Map()).format)
    pprintln(stepAll(Configuration(program, Store(Map()))))
  }

}
