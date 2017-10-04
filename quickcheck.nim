import macros, breeze
import strutils, sequtils, tables, intsets, future

when not defined(js):
  import random.urandom, random.xorshift

  type
    Engine = SystemRandom
  var engine*: Engine = initSystemRandom()
  proc initRng: Engine =
    engine


else:
  import jsffi

  type
    Engine* = ref object of js
      engine*: js

  proc randomInt*(a: var Engine, max: int): int =
    cast[int](a.engine.integer(0, max))

  var require* {.importc.}: (cstring) -> js
  var jsrandom = require(cstring"random-js")
  var console* {.importc.}: js
  proc a*: js {.importcpp: "function(){var random = require('random-js');return new random(random.engines.mt19937().autoSeed())}()".}
  var engine*: Engine = Engine(engine: a())

  proc initRng: Engine =
    engine


proc generateQuicktest*(args: NimNode): NimNode

macro ObjectGen*(t: typed, args: untyped): untyped =
  var typ = getType(t)
  if typ.kind == nnkBracketExpr and typ[1].kind == nnkBracketExpr and $typ[1][0] == "ref":
    typ = getType(typ[1][1])
  echo treerepr(typ)
  echo treerepr(args)
  var name = args[0]
  var call = args[1][0]
  result = quote:
    echo 0
  # var gen = nnkExprColonExpr.newTree(
  #   name,



macro quicktest*(args: untyped): untyped =
  result = generateQuicktest(args)
  var name = args[0]
  result = quote:
    test `name`:
      `result`
  echo repr(result)

proc replaceNames(node: var NimNode, names: seq[string]) =
  var z = 0
  for s in node:
    var son = node[z]
    if son.kind in {nnkSym, nnkIdent}:
      if $son in names:
        node[z] = newIdentNode(!("$1Gen" % $son))
    else:
      replaceNames(son, names)
    inc z

proc generateGenerator(generator: NimNode, names: seq[string]): (NimNode, NimNode, NimNode) =
  assert generator.kind == nnkIdentDefs
  var ident = generator[0]
  var identGen = newIdentNode(!("$1Gen" % $(generator[0])))
  var expression = generator[1]
  if generator[1].kind == nnkPrefix and $generator[1][0] == "!":
    if generator[1][1].kind == nnkCall:
      expression = nnkCall.newTree(nnkPrefix.newTree(generator[1][0], generator[1][1][0]))
      var z = 0
      for g in generator[1][1]:
        if z > 0:
          expression.add(g)
        inc z
  replaceNames(expression, names)
  var generatorName: string
  var args: seq[NimNode] = @[]
  var isBuiltin = true
  if expression.kind == nnkIdent:
    generatorName = $expression
  elif expression.kind == nnkPrefix and $expression[0] == "!":
    generatorName = $expression[1]
    isBuiltin = false
  else:
    if expression[0].kind == nnkPrefix and $expression[0][0] == "!":
      generatorName = $(expression[0][1])
      isBuiltin = false
    elif expression.kind == nnkBracketExpr and $expression[0] == "seq":
      generatorName = "Type"
      var element = expression[1]
      if element.kind == nnkIdent:
        var e = quote:
          Type[`element Gen`]()
        args = @[e]
      else:
        element = element[0]
        var t = quote:
          Type(`element`)
        t = nnkExprEqExpr.newTree(newIdentNode(!"element"), t)
        args = @[t]
        var z = 0
        for x in expression[1]:
          if z > 0:
            t[1][0].add(x)
          inc z        
      var a = expression[0]
      var t = quote:
        `a`[`element`]
      args = concat(@[t], args)
    else:
      generatorName = $(expression[0])
    if len(args) == 0:
      var z = 0
      for arg in expression:
        if z > 0:
          args.add(arg)
        inc z
  if isBuiltin and generatorName != "Type":
    args = concat(@[newIdentNode(!($generatorName))], args)
    generatorName = "Type"
  var generatorNameNode = newIdentNode(!generatorName)
  if generator[1].kind == nnkObjConstr:
    result[0] = quote:
      var `identGen` = ObjectGen()
  else:
    result[0] = quote:
      var `identGen` = `generatorNameNode`()
  for arg in args:
    result[0][0][0][2].add(arg)
  result[1] = quote:
    var `ident` = `identGen`.generate()
    `identGen`.last = `ident`

  var s = newLit($generator[0])
  var error = newIdentNode(!"error")
  result[2] = quote:
    `error`.add("[" & `s` & "] " & $(`ident`) & "\n")

proc generateQuicktest*(args: NimNode): NimNode =
  var label = $args[0]
  var generators = toSeq(args[1][3])
  generators.keepItIf(it.kind != nnkEmpty)
  var generatorNames = generators.mapIt($it[0])
  var gens = generators.mapIt(generateGenerator(it, generatorNames))
  result = buildMacro:
    stmtList()
  result = result[0]
  var init = buildMacro:
    stmtList()
  init = init[0]
  var error = newIdentNode(!"error")
  var checkpoint = quote:
    var `error` = "values:\n"

  for gen in gens:
    result.add(gen[0])
    init.add(gen[1])
    checkpoint.add(gen[2])

  var acheckpoint = newIdentNode(!"checkpoint")
  var e = quote:
    `acheckpoint`(`error`)
  checkpoint.add(e)
  var test = args[^1][^1]
  var generatedTest = quote:
    for z in 0..<50:
      `init`
      `checkpoint`
      `test`

  result.add(generatedTest)

type
  Alphabet* = enum AUndefined, AAll, AAscii, ANone

  Arbitrary*[T] = concept a
    arbitrary(type a) is Gen[T]

  Gen*[T] = ref object {.inheritable.}
    rng*:       Engine
    last*:      T
    fil*:       FilterMixin[T]

  LimitMixin*[T] = ref object
    min*:       T
    max*:       T

  FilterMixin*[T] = ref object
    test*:      (T) -> bool
    trans*:     (T) -> T

  StringGen* = ref object of Gen[string]
    limit*:     LimitMixin[int]
    symbols:    seq[char]
    alphabet:   Alphabet

  BoolGen* = ref object of Gen[bool]

  CharGen* = ref object of Gen[char]

  FloatGen* = ref object of Gen[float]
    limit*:     LimitMixin[float]

  SeqGen*[T] = ref object of Gen[seq[T]]
    # slimit*:    LimitMixin[int]
    # sfil*:      FilterMixin[seq[T]]
    limit*:     LimitMixin[int]
    # last*:      seq[T]
    element*:   Gen[T]
    infer*:     bool
    # rng*:       Engine

  InsideGen* = ref object of Gen[string]
    limit*:     LimitMixin[int]
    inside*:    StringGen

  IntGen* = ref object of Gen[int]
    limit*:     LimitMixin[int]
    skip*:      IntSet

type
  Iterator[T] = concept a
    for element in a:
      element is T

  WithLimit[T] = concept a
    a.limit is LimitMixin[T]

  WithFilter[T] = concept a
    a.fil is FilterMixin[T]

proc min[T](a: WithLimit[T]): T =
  a.limit.min

proc max[T](a: WithLimit[T]): T =
  a.limit.max

proc test*[T](a: WithFilter[T]): (T) -> bool =
  a.fil.test

proc trans*[T](a: WithFilter[T]): (T) -> T =
  a.fil.trans

proc arbitrary*(t: typedesc[int]): IntGen =
  IntGen()

proc arbitrary*(t: typedesc[string]): StringGen =
  StringGen()

proc arbitrary*(t: typedesc[bool]): BoolGen =
  BoolGen()

proc arbitrary*(t: typedesc[char]): CharGen =
  CharGen()

proc arbitrary*(t: typedesc[float]): FloatGen =
  FloatGen()

proc arbitrary*[T](t: typedesc[seq[T]]): SeqGen[T] =
  SeqGen[T]()

proc arbitrary*(t: typedesc[IntGen]): IntGen =
  IntGen()

proc arbitrary*(t: typedesc[StringGen]): StringGen =
  StringGen()

proc arbitrary*(t: typedesc[BoolGen]): BoolGen =
  BoolGen()

proc arbitrary*(t: typedesc[CharGen]): CharGen =
  CharGen()

proc arbitrary*(t: typedesc[FloatGen]): FloatGen =
  FloatGen()

proc arbitrary*[T](t: typedesc[SeqGen[T]]): SeqGen[T] =
  SeqGen[T]()


proc randomIn*(rng: var Engine, min: int, max: int): int =
  if min == low(int):
    return randomInt(rng, high(int))
  return randomInt(rng, max + (1 - min)) + min

proc choice*[T](rng: var Engine, elements: seq[T]): T =
  result = elements[randomInt(rng, len(elements))]

proc toSeq*[T](s: set[T]): seq[T] =
  result = @[]
  for c in s:
    result.add(c)

proc generateSequence*[T](rng: var Engine, elements: seq[T], min: int, max: int): seq[T] =
  result = @[]
  var length = randomIn(rng, min, max)
  for element in 0..<length:
    result.add(choice(rng, elements))

proc generateSequence*[T](rng: var Engine, generator: (int) -> T, limit: int, min: int, max: int): seq[T] =
  result = @[]
  var length = randomIn(rng, min, max)
  for element in 0..<length:
    var number = randomInt(rng, limit)
    result.add(generator(number))

proc String*(symbols: set[char], test: (string) -> bool = nil, trans: (string) -> string = nil, min: int = 0, max: int = 20): StringGen =
  result = StringGen(limit: LimitMixin[int](min: min, max: max), fil: FilterMixin[string](test: test, trans: trans), symbols: toSeq(symbols), alphabet: AUndefined, last: "", rng: initRng())

proc String*(alphabet: Alphabet = AAll, min: int = 0, max: int = 20): StringGen =
  result = StringGen(limit: LimitMixin[int](min: min, max: max), fil: FilterMixin[string](test: nil, trans: nil), alphabet: alphabet, last: "", rng: initRng())


proc uniq(skip: seq[int]): IntSet =
  result = initIntSet()
  for s in skip:
    result.incl(s)

proc Type*(
    t: typedesc[string],
    test: (string) -> bool = nil,
    trans: (string) -> string = nil,
    min: int = 0,
    max: int = 20,
    alphabet: Alphabet = AUndefined): StringGen =
  result = StringGen(limit: LimitMixin[int](min: min, max: max), fil: FilterMixin[string](test: test, trans: trans), alphabet: alphabet, last: "", rng: initRng())

proc Type*(
    t: typedesc[int],
    test: (int) -> bool = nil,
    trans: (int) -> int = nil,
    min: int = 0,
    max: int = 20): IntGen =
  result = IntGen(limit: LimitMixin[int](min: min, max: max), fil: FilterMixin[int](test: test, trans: trans), skip: uniq(@[]), rng: initRng())

proc Type*(
    t: typedesc[bool],
    test: (bool) -> bool = nil,
    trans: (bool) -> bool = nil): BoolGen =
  result = BoolGen(fil: FilterMixin[bool](test: test, trans: trans), rng: initRng())

proc Type*(
    t: typedesc[char],
    test: (char) -> bool = nil,
    trans: (char) -> char = nil): CharGen =
  result = CharGen(fil: FilterMixin[char](test: test, trans: trans), rng: initRng())

proc Type*(
    t: typedesc[float],
    test: (float) -> bool = nil,
    trans: (float) -> float = nil,
    min: float = 0.0,
    max: float = 20.0): FloatGen =
  result = FloatGen(limit: LimitMixin[float](min: min, max: max), fil: FilterMixin[float](test: test, trans: trans), rng: initRng())

proc Type*[T](
    t: typedesc[seq[T]],
    test: (seq[T]) -> bool = nil,
    trans: (seq[T]) -> seq[T] = nil,
    min: int = 0,
    max: int = 20): SeqGen[T] =
  result = SeqGen[T](element: arbitrary(T), infer: true, limit: LimitMixin[int](min: min, max: max), fil: FilterMixin[seq[T]](test: test, trans: trans), rng: initRng())


proc Type*[T](
    t: typedesc[seq[T]],
    element: Arbitrary[T],
    test: (seq[T]) -> bool = nil,
    trans: (seq[T]) -> seq[T] = nil,
    min: int = 0,
    max: int = 20): SeqGen[T] =
  result = SeqGen[T](element: element, infer: false, limit: LimitMixin[int](min: min, max: max), fil: FilterMixin[seq[T]](test: test, trans: trans), rng: initRng())

proc generateInternal*(g: var StringGen): string =
  var chars: seq[char]
  if g.alphabet == AUndefined:
    chars = generateSequence(g.rng, g.symbols, g.min, g.max)
    result = chars.join()
  else:
    var generator: (int) -> char
    var limit: int
    case g.alphabet:
    of AAll:
      generator = (number: int) => chr(number)
      limit = 256
    of AAscii:
      generator = (number: int) => chr(number)
      limit = 128
    of ANone, AUndefined:
      raise newException(ValueError, "None")

    chars = generateSequence(g.rng, generator, limit, g.min, g.max)
    result = chars.join()

proc Inside*(s: StringGen, min: int = 0, max: int = 10): InsideGen =
  result = InsideGen(limit: LimitMixin[int](min: min, max: max), inside: s, rng: initRng())

proc generateInternal*(g: var InsideGen): string =
  var length = randomIn(g.rng, g.min, min(g.max, max(g.min + 1, len(g.inside.last))))
  var first = randomIn(g.rng, 0, max(1, len(g.inside.last) - length))
  result = g.inside.last[first..first + length - 1]
  if len(result) < g.min:
    result.add(repeat(" ", g.min - len(result)))

proc Int*(min: int = low(int), max: int = high(int), skip: seq[int] = @[]): IntGen =
  result = IntGen(limit: LimitMixin[int](min: min, max: max), skip: uniq(skip), rng: initRng())

proc generateInternal*(g: var IntGen): int =
  var started = false
  while not started or result in g.skip:
    started = true
    result = randomIn(g.rng, g.min, g.max)

proc generateInternal*(g: var BoolGen): bool =
  result = bool(randomIn(g.rng, 0, 1))

proc generate*[T](g: var WithFilter[T]): T =
  var started = false
  while not started or not (g.test() == nil or g.test()(result)):
    started = true
    result = generateInternal(g)
    if g.trans() != nil:
      result = g.trans()(result)

proc generate*[T](g: var SeqGen[T]): seq[T] =
  var started = false
  while not started or not (g.test() == nil or g.test()(result)):
    started = true
    result = generateInternal(g)
    if g.fil.trans != nil:
      result = g.fil.trans(result)

proc generateInternal*(g: var CharGen): char =
  result = chr(randomIn(g.rng, 0, 127))

proc generateInternal*(g: var FloatGen): float =
  result = float(randomIn(g.rng, int(g.min), int(g.max)))


proc generateInternal*[T](g: var SeqGen[T]): seq[T] =
  var length = randomIn(g.rng, g.limit.min, g.limit.max)
  result = @[]
  for z in 0..<length:
    echo z
    if g.infer:
      var t = arbitrary(T)
      result.add(t.generate())
    else:
      var t = cast[type(arbitrary(T))](g.element) # are you fucking kidding me
      result.add(t.generate())
    

echo Type(int, min = 0, max = 20) is IntGen
echo arbitrary(typedesc[IntGen]) is Gen[int]
# ok
# fix the generator
