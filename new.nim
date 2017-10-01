import strutils, sequtils, tables, intsets
import macros, future

type
  Engine* = concept a
    randomInt(var a, int) is int

when not defined(js):
  import random

  type
    JSEngine* = ref object

    NimEngine* = ref object

  proc randomInt*(e: var NimEngine, max: int): int =
    result = random(max)

  proc newEngine*(seed: int): Engine =
    randomize(seed)
    result = NimEngine()


else:
  import jsffi

  type
    JSEngine* = ref object of js
      engine*: js

    NimEngine* = ref object of js

  proc randomInt*(a: var JSEngine, max: int): int =
    cast[int](a.engine.integer(0, max))

  var require* {.importc.}: proc (module: cstring): js
  var jsrandom = require(cstring"random-js")
  var console* {.importc.}: js
  proc a*(seed: int): js {.importcpp: "function(){var random = require('random-js');return new random(random.engines.mt19937().seed(#))}()".}

  proc newEngine*(seed: int): JSEngine =
    JSEngine(engine: a(seed))

proc randomIn*(rng: var Engine, min: int, max: int): int =
  if min == low(int):
    return randomInt(rng, high(int))
  return randomInt(rng, max + (1 - min)) + min

type
  QResultKind* = enum Success, Failure

  QResult* = ref object
    case kind*: QResultKind
    of Success:
      discard
    of Failure:
      seed*: int
      counterExample*: seq[string]

  Gen*[T] = ref object {.inheritable.}
    # when not defined(js):
    #   rng*:     SystemRandom
    # else:
    #   rng*:     JSEngine  
    last*:      T

  Arbitrary*[T] = concept a
    arbitrary(type a) is Gen[T]

  Property* = ref object
    resGen*: Gen[QResult]

  Testable* = concept a
    toProperty(a) is Property

  LimitMixin* = ref object
    min*:       int
    max*:       int

  WithLimit* = concept a
    a.limit is LimitMixin

proc `$`*(res: QResult): string =
  result = case res.kind:
    of Success:
      "Success()"
    of Failure:
      "Failure($1)" % res.counterExample.join(", ")

proc `==`*(a: QResult, b: QResult): bool =
  if a.kind != b.kind:
    result = false
  elif a.kind == Success:
    result = true
  elif a.kind == Failure and b.kind == Failure:
    result = a.counterExample == b.counterExample

proc overFailure*(res: QResult, on: (QResult) -> QResult): QResult =
  result = case res.kind:
    of Success:
      res
    of Failure:
      on(res)

method generate*[T](gen: Gen[T], engine: Engine): QResult =
  QResult(kind: Success)

type
  IntGen* = ref object of Gen[int]
    limit*: LimitMixin

  BoolGen* = ref object of Gen[bool]

  StringGen* = ref object of Gen[string]
    limit*: LimitMixin

proc `$`*(i: IntGen): string =
  "IntGen()"

proc `$`*(b: BoolGen): string =
  "BoolGen()"

proc `$`*(s: StringGen): string =
  "StringGen()"

proc arbitrary*(t: typedesc[int]): Gen[int] =
  result = IntGen()

proc arbitrary*(t: typedesc[bool]): Gen[bool] =
  result = BoolGen()

proc arbitrary*(t: typedesc[string]): Gen[string] =
  result = StringGen()

method generate*(intGen: IntGen, engine: Engine): int =
  result = randomIn(engine, 0, high(int))

method generate*(boolGen: BoolGen, engine: Engine): bool =
  result = bool(randomIn(engine, 0, 1))

method generate*(stringGen: StringGen, engine: Engine): bool =
  var length = randomIn(engine, 0, 20)
  result = ""
  for z in length:
    result.add(chr(randomIn(engine, 0, 255)))


proc runProperty*(property: Property, engine: Engine): QResult =
  result = property.resGen.generate(engine)

proc toProperty*(p: Property): Property =
  result = p

proc toProperty*(res: QResult): Property =
  result = Property(resGen: Gen[QResult](last: res))

proc toResult*(b: bool): QResult =
  result = if b: QResult(kind: Success) else: QResult(kind: Failure, counterExample: @[])

proc toProperty*(b: bool): Property =
  result = toProperty(toResult(b))

proc split(engine: Engine): (Engine, Engine) =
  result = (engine, engine)

# proc forAll*[T](gen: Gen[T], testable: (Arbitrary[T]) -> Testable): Property =
#   var (rand1, rand2) = split(gen.engine)
#   var arg = gen.generate(rand1)
#   var subTestable = Property(resGen: testable(arg))
#   var res = runProperty(subTestable, rand2)
#   # var res = QResult(kind: Success)
#   result = overFailure(res, proc(res: QResult): QResult = QResult(kind: Failure, seed: -1, counterExample: concat(res.counterExample, @[$arg])))

proc forAll*(gen: Gen, testable: (Arbitrary) -> Testable): Property =
  echo 0

proc toProperty*[T](testable: proc (a: T): Testable): Property =
  # discard
  # var t: T
  # var gen = arbitrary(typedesc[T])
  # result = forAll(StringGen(), testable)
  assert T is Arbitrary[U]
  result = forAll(arbitrary(T), testable)

proc runOne*(property: Property, seed: int): QResult =
  var res = runProperty(property, newEngine(seed))
  result = overFailure(res, proc(res: QResult): QResult = QResult(kind: Failure, seed: seed, counterExample: res.counterExample))

proc runAll*(attempt: int, seed: int, property: Property): QResult =
  result = QResult(kind: Success)
  for z in seed..seed + attempt - 1:
    var res = runOne(property, z)
    result = case result.kind:
      of Failure:
        result
      of Success:
        res


proc newCheckImpl*(attempt: int, seed: int, property: Testable): QResult =
  result = runAll(attempt, seed, toProperty(property))

proc newCheck*(property: Testable): QResult =
  var seed = 5
  result = newCheckImpl(100, seed, property)


proc a0(separator: string): bool =
  result = "z".split(separator).join(separator) == "z"
  
# proc testSplit(s: string): (string) -> bool =
#   proc a0(separator: string): bool =
#     result = s.split(separator).join(separator) == s
#   result = a0

type t = (string) -> bool
echo t is Testable
echo arbitrary(string) is Gen[string]
# echo string is Arbitrary[string]
# echo newCheck(a0)
