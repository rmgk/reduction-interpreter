package de.rmgk.tfl

import scala.language.implicitConversions

object Tfl {
  sealed trait Term {
    def apply(argument: Term) = App(this, argument)
    def =>:(identifier: Identifier): Fun = Fun(identifier, this)
    def fold(value: Term)(operator: Term) = Fold(this, value, operator)
    def value = Access(this)
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

  case class Reactive(id: Int) extends Value {
    override def toString: String = s"r$id"
  }
  object Reactive {
    var counter = 0
    def fresh(): Reactive = {
      counter = counter + 1
      Reactive(counter)
    }
  }
  case class React(input1: Term, input2: Term, operator: Term) extends Term
  case object Evt extends Term
  def Var(init: Term): Term = Let('aVal, Evt, pair('aVal)('aVal.fold(init){'_ =>: 'new =>: 'new}) )
  case class Fold(input: Term, initial: Term, operator: Term) extends Term {
    override def toString(): String = s"Fold(${input.toString()}, ${initial.toString}, ${operator.toString})"
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
                    inputs    : List[Reactive],
                    outputs   : Set[Reactive],
                    continuous: Boolean) {
    def addOutput(o: Reactive): Stored = copy(outputs = outputs + o)
  }
  object Store {
    def Evt() = Stored(unit, 0, unit, Nil, Set.empty, continuous = false)
    def Fold(self: Reactive, inputI: Reactive, initialI: Stuck, operatorI: Stuck): Stored =
      Stored(initialI, 0, operatorI, List(self, inputI), Set.empty, continuous = true)
    def React(inputs: List[Reactive], operator: Stuck) = Stored(unit, 0, operator, inputs, Set.empty, continuous = true)
  }

  case class Store(mapping: Map[Reactive, Stored]) {
    def isEmpty: Boolean = mapping.isEmpty
    def apply(r: Reactive): Stored = mapping.apply(r)
    def update(r: Reactive, v: Stuck): Store = copy(mapping.updated(r, mapping(r).copy(value = v)))
    def value(r: Reactive): Stuck = mapping.get(r).map(_.value).getOrElse(Error(s"unknown reactive $r"))
    def addDependencies(r: Reactive, inputs: Iterable[Reactive]): Map[Reactive, Stored] = {
      inputs.filter(_ != r).foldLeft(mapping){(acc, i) =>
        acc.updated(i, acc(i).addOutput(r))
      }
    }
    def add(r: Reactive, stored: Stored): Store = {
      assert(!mapping.contains(r), s"duplicate add $r")
      Store(addDependencies(r, stored.inputs).updated(r, stored))
    }
    def reactives: Set[Reactive] = mapping.keySet
  }

//  case class Closure(fun: Fun, environment: Environment) extends Value {
//    override def format(): String = s"Closure(${fun.format()},$environment)"
//    override def tex(): String = s"Closure(${fun.tex()},$environment)"
//  }

  def freeVariables(term: Term): Set[Identifier] = term match {
    case id: Identifier                 => Set(id)
    case Fun(parameter, body)           => freeVariables(body) - parameter
    case App(function, argument)        => freeVariables(function) ++ freeVariables(argument)
    case TryCatch(body, handler)        => freeVariables(body) ++ freeVariables(handler)
    case Fold(input, initial, operator) => freeVariables(input) ++ freeVariables(initial) ++ freeVariables(operator)
    case React(i1, i2, op)              => freeVariables(i1) ++ freeVariables(i2) ++ freeVariables(op)
    case Access(target)                 => freeVariables(target)
    case _: Text |
         _: Error |
         _: Reactive |
         Evt                            => Set()
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

      case Evt =>
        val r = Reactive.fresh()
        InterpreterResult(r, store.add(r, Store.Evt()))

      case Fold(input, initial, operator) =>
        interpret(input, store) match {
          case InterpreterResult(input : Reactive, inStore) =>
            val initialI = interpret(initial, inStore)
            val operatorI = interpret(operator, initialI.store)
            val r = Reactive.fresh()
            InterpreterResult(r, store.add(r, Store.Fold(r, input, initialI.value, operatorI.value)))
          case other => other.copy(Error(s"can not fold ${other.value}"))
        }

      case react: React => ???

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
//    val freeVar = freeVariables(argument)
//    assert(freeVar == Set(), s"unbound identifier $freeVar")

    def subsi(t: Term) = subs(t, parameter, argument)

    term match {
      case id: Identifier                  => if (id == parameter) argument else id
      case fun@Fun(param, body)            =>
        if (param == parameter) fun else
                                      Fun(param, subsi(body))
      case App(functionTerm, argumentTerm) =>
        App(subs(functionTerm, parameter, argument), subsi(argumentTerm))
      case Access(target)                  => Access(subsi(target))
      case TryCatch(body, handler)         => TryCatch(subsi(body), subsi(handler))
      case Fold(a, b, c)                   => Fold(subsi(a), subsi(b), subsi(c))
      case React(a, b, c)                  => React(subsi(a), subsi(b), subsi(c))
      case _: Text |
           _: Error |
           _: Reactive |
           _: Identifier |
           Evt                             => term
    }
  }

  case class UpdatePropagation(outdated: Set[Reactive], finalized: Set[Reactive]) {
    def reevaluate(toEval: Reactive, outputs: Set[Reactive]): Some[UpdatePropagation] =
      Some(UpdatePropagation(outdated ++ outputs - toEval, finalized + toEval))

    def skip(reactives: Set[Reactive]): Some[UpdatePropagation] = Some(copy(finalized = finalized ++ reactives))
  }

  case class Configuration(term: Term,
                           store: Store,
                           propagation: Option[UpdatePropagation],
                           reevaluation: Option[(Term, Reactive)]
                          ) {
    def derive(name         : String,
               toTerm       : Term = term,
               premises     : List[Rule] = Nil,
               toStore      : Store = store,
               toPropagation: Option[UpdatePropagation] = propagation,
               toReevaluate : Option[(Term, Reactive)] = reevaluation
              )
    : Some[Rule] = {
      Some(Rule(name, this, Configuration(toTerm, toStore, toPropagation, toReevaluate), premises))
    }
    def format(): String = s"${term.toString()} {${store.toString}}"
    def tex(): String = term.tex()
  }
  object Configuration {
    def apply(term: Term): Configuration = Configuration(term, Store(Map()), None, None)
  }

  case class Rule(name: String, from: Configuration, to: Configuration, premises: List[Rule] = Nil) {
    def tex(): String =
      s"\\infer{${premises.map(_.tex).mkString("{", "} \\and {", "}")}}{${from.tex}\\\\ → ${to.tex}} \\named{$name}"
    def format(): String =
      s"${to.format} [$nameTree]"
    def nameTree(): String = s"$name [${premises.map(_.nameTree()).mkString(",")}]"
  }


  def stepTerm(conf: Configuration): Option[Rule] = {
    def context(term: Term, outer: Term => Term): Option[Rule] = {
      term match {
        case err: Error => conf.derive("error", err)
        case nonError   =>
          stepTerm(conf.copy(nonError)).flatMap { inner =>
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

      // firing of events
      case App(r: Reactive, v: Value) =>
        conf.derive("fire", r, toStore = conf.store(r) = v,
                    toPropagation = Some(UpdatePropagation(conf.store(r).outputs, Set(r))))

      case Access(target: Reactive) => conf.derive("access", conf.store.value(target))

      case Evt =>
        val r = Reactive.fresh()
        conf.derive("var", r, toStore = conf.store.add(r, Store.Evt()))

      case React(input1: Reactive, input2: Reactive, operator: Value) =>
        val r = Reactive.fresh()
        val stored = Store.React(List(input1, input2), operator)
        conf.derive("init",
                    toTerm = r,
                    toStore = conf.store.add(r, stored),
                    toReevaluate = Some((applyOperator(conf, stored), r)))


      case Fold(input: Reactive, initial: Value, operator: Fun) =>
        val r = Reactive.fresh()
        conf.derive("fold", r, toStore = conf.store.add(r, Store.Fold(r, input, initial, operator)))


      case tc@TryCatch(body, _)        => context(body, inner => tc.copy(body = inner))
      case app@App(_: Value, argument) => context(argument, inner => app.copy(argument = inner))
      case app@App(function, _)        => context(function, inner => app.copy(function = inner))
      case acc@Access(target)          => context(target, inner => acc.copy(target = inner))

      case f@Fold(_: Reactive, _: Value, term) => context(term, inner => f.copy(operator = inner))
      case f@Fold(_: Reactive, term, _)        => context(term, inner => f.copy(initial = inner))
      case f@Fold(term, _, _)                  => context(term, inner => f.copy(input = inner))

      case r@React(_: Reactive, _: Reactive, term) => context(term, inner => r.copy(operator = inner))
      case r@React(_: Reactive, term, _)           => context(term, inner => r.copy(input2 = inner))
      case r@React(term, _, _)                     => context(term, inner => r.copy(input1 = inner))

    }
  }

  def stepPropagation(conf: Configuration): Option[Rule] = {

    def ready(finalized: Set[Reactive])(reactive: Reactive): Boolean =
      conf.store(reactive).inputs.filter(_ != reactive).toSet.subsetOf(finalized)

    conf.propagation.flatMap {
      case currentPropagation@UpdatePropagation(outdated, finalized) =>
        if (outdated.isEmpty) conf.derive("clean", toPropagation = None)
        else {
          val skippable = conf.store.reactives.filter(ready(finalized)) -- outdated -- finalized
          if (skippable.nonEmpty) {
            conf.derive("skip", toPropagation = currentPropagation.skip(skippable))
          }
          else {
            outdated.find(ready(finalized)).flatMap { toEval =>
              val stored = conf.store(toEval)
              val nextPropagation = currentPropagation.reevaluate(toEval, stored.outputs)
              conf.derive("reevaluate",
                          toReevaluate = Some((applyOperator(conf, stored), toEval)),
                          toPropagation = nextPropagation)
            }
          }
        }
    }
  }

  private def applyOperator(conf: Configuration, stored: Stored): Term = {
    val inValues = stored.inputs.map(conf.store.value)
    inValues.foldLeft(stored.operator) { (op, v) => op(v) }
  }

  def stepReevaluation(conf: Configuration): Option[Rule] = {
    conf.reevaluation.flatMap {
      case (term: Stuck, reactive) => conf.derive("evaluated",
                                                  toStore = conf.store(reactive) = term,
                                                  toReevaluate = None)
      case (term, reactive) =>
        stepTerm(Configuration(term)).flatMap { inner =>
        assert(inner.to.store.isEmpty, "inner evaluation had side effects")
          conf.derive("inner", premises = List(inner),
                          toReevaluate = Some((inner.to.term, reactive)))
        }
    }
  }


  def step(conf: Configuration): Option[Rule] = {
    conf match {
      case Configuration(term, store, propagation @ None, reevaluation @ None) => stepTerm(conf)
      case Configuration(term, store, propagation, reevaluation @ None) => stepPropagation(conf)
      case Configuration(term, store, propagation, reevaluation) => stepReevaluation(conf)
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

    def evaluateFun(term: Term, store: Store, acc: Int): Either[Term, Int] = {
      val res = interpret(term, store)
      res.value match {
        case `zero` => Right(acc)

        case Fun(Identifier('succ), Fun(Identifier('base), App(Identifier('succ), t))) =>
          evaluateFun(subs(subs(t, 'base, zero), 'succ, succ), store, acc + 1)

        case other =>
          if (acc != 0) println(s"warning, not quite a numerical structure? at $acc + $res")
          evaluateFun(other(succ)(zero), store, acc)
      }
    }

    evaluateFun(value, Store(Map()), 0)
  }


  val id    : Fun = 'id =>: 'id
  val zero  : Fun = 'zero =>: id
  val unit  : Fun = zero
  val one   : Fun = id
  val succ  : Fun = 'a =>: 'succ =>: 'base =>: 'succ ('a ('succ)('base))
  val add   : Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a ('f)('b ('f)('x))
  val two   : App = add(one)(one)
  val times : Fun = 'a =>: 'b =>: 'f =>: 'x =>: 'a ('b ('f))('x)
  val four  : App = times(two)(two)
  val ten   : App = times(succ(four))(two)
  val pair  : Fun = 'first =>: 'second =>: 'res =>: 'res('first)('second)
  val first : Fun = 'pair =>: 'pair('f =>: '_ =>: 'f)
  val second: Fun = 'pair =>: 'pair('_ =>: 's =>: 's)
  val now   : Fun = 'var =>: second('var).value
  val set   : Fun = first

  def account: Term =
    Let('balance, Var(one),
        Let('deposit, 'amount =>: set('balance)(add(now('balance))('amount)),
            Let('multiplier, Var(two),
                Let('result, React(second('deposit), second('multiplier), times),
                    execute(
                      'deposit (one),
                      'deposit (two),
                      'deposit (ten),
                      set('multiplier)(ten),
                      Access('result)
                    )
                )
            )
        )
    )


  def main(args: Array[String]): Unit = {
    val program = account
//    val program = times(add(one)(two))(one(add(succ(two))(times(two)(succ(two)))))
    println(program.toString())

    val res = stepAll(Configuration(program))
    println(toInt(res.term))
    println(toInt(first(pair)))
//    pprintln(toInt(times(two)(ten)))
//    val res = interpret(program, Map())
//    pprintln(res)
//    pprintln(substitute(substitute(res, Map()), Map()))
//    println(program.tex())
//    pprintln(interpret(program, Map()))
//    println(interpret(program("1")("0"), Map()).format)
  }

}
