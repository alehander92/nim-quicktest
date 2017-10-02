import macros, breeze
import strutils, sequtils, tables, intsets

type
  Engine* = concept a
    randomInt(var a, int) is int

when not defined(js):
  import random.urandom, random.xorshift

  type
      JSEngine* = ref object

  var engine*: SystemRandom = initSystemRandom()

  proc initRng: SystemRandom =
    engine


else:
  import jsffi

  type
    JSEngine* = ref object of js
      engine*: js

    SystemRandom* = ref object of js

  proc randomInt*(a: var JSEngine, max: int): int =
    cast[int](a.engine.integer(0, max))

  var require* {.importc.}: proc (module: cstring): js
  var jsrandom = require(cstring"random-js")
  var console* {.importc.}: js
  proc a*: js {.importcpp: "function(){var random = require('random-js');return new random(random.engines.mt19937().autoSeed())}()".}
  var engine*: JSEngine = JSEngine(engine: a())

  proc initRng: JSEngine =
    engine


proc generateQuicktest*(args: NimNode): NimNode

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
  var generatorName: string
  var args: seq[NimNode] = @[]
  var isBuiltin = true
  if expression.kind == nnkIdent:
    generatorName = $expression
  elif expression.kind == nnkPrefix and $expression[0] == "!":
    generatorName = $expression[1]
    isBuiltin = false
  else:
    generatorName = "$1" % $(expression[0])
    args = toSeq(expression)
    args.keepItIf(it.kind != nnkIdent)
  if isBuiltin:
    args = concat(@[newIdentNode(!($generatorName))], args)
    generatorName = "Type"
  replaceNames(expression, names)
  var generatorNameNode = newIdentNode(!generatorName)
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

  Gen*[T] = ref object {.inheritable.}
    when not defined(js):
      rng*:     SystemRandom
    else:
      rng*:     JSEngine  
    last*:      T

  LimitMixin* = ref object
    min*:       int
    max*:       int

  StringGen* = ref object of Gen[string]
    limit*:     LimitMixin
    symbols:    seq[char]
    alphabet:   Alphabet
    # last*:      string
    # rng*:       SystemRandom

  InsideGen* = ref object of Gen[string]
    limit*:     LimitMixin
    inside*:    StringGen
    # last*:      string
    # rng*:       SystemRandom

  IntGen* = ref object of Gen[int]
    limit*:     LimitMixin
    skip*:      IntSet

type
  Iterator[T] = concept a
    for element in a:
      element is T

  WithLimit = concept a
    a.limit is LimitMixin

proc min(a: WithLimit): int =
  a.limit.min

proc max(a: WithLimit): int =
  a.limit.max

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

proc generateSequence*[T](rng: var Engine, generator: proc(number: int): T, limit: int, min: int, max: int): seq[T] =
  result = @[]
  var length = randomIn(rng, min, max)
  for element in 0..<length:
    var number = randomInt(rng, limit)
    result.add(generator(number))

proc String*(symbols: set[char], min: int = 0, max: int = 20): StringGen =
  result = StringGen(limit: LimitMixin(min: min, max: max), symbols: toSeq(symbols), alphabet: AUndefined, last: "", rng: initRng())

proc String*(alphabet: Alphabet = AAll, min: int = 0, max: int = 20): StringGen =
  result = StringGen(limit: LimitMixin(min: min, max: max), alphabet: alphabet, last: "", rng: initRng())


proc uniq(skip: seq[int]): IntSet =
  result = initIntSet()
  for s in skip:
    result.incl(s)

proc Type*(t: typedesc[string], min: int = 0, max: int = 20): StringGen = 
  result = StringGen(limit: LimitMixin(min: min, max: max), alphabet: AAll, last: "", rng: initRng())

proc Type*(t: typedesc[int], min: int = 0, max: int = 20): IntGen =
  result = IntGen(limit: LimitMixin(min: min, max: max), skip: uniq(@[]), rng: initRng())

proc generate*(g: var StringGen): string =
  var chars: seq[char]
  if g.alphabet == AUndefined:
    chars = generateSequence(g.rng, g.symbols, g.min, g.max)
  else:
    var generator: proc(number: int): char
    var limit: int
    case g.alphabet:
    of AAll:
      generator = proc(number: int): char = chr(number)
      limit = 256
    of AAscii:
      generator = proc(number: int): char = chr(number)
      limit = 128
    of ANone, AUndefined:
      raise newException(ValueError, "None")

    chars = generateSequence(g.rng, generator, limit, g.min, g.max)
  result = chars.join()

proc Inside*(s: StringGen, min: int = 0, max: int = 10): InsideGen =
  result = InsideGen(limit: LimitMixin(min: min, max: max), inside: s, rng: initRng())

proc generate*(g: var InsideGen): string =
  var length = randomIn(g.rng, g.min, min(g.max, max(g.min + 1, len(g.inside.last))))
  var first = randomIn(g.rng, 0, max(1, len(g.inside.last) - length))
  result = g.inside.last[first..first + length - 1]
  if len(result) < g.min:
    result.add(repeat(" ", g.min - len(result)))

proc Int*(min: int = low(int), max: int = high(int), skip: seq[int] = @[]): IntGen =
  result = IntGen(limit: LimitMixin(min: min, max: max), skip: uniq(skip), rng: initRng())

proc generate*(g: var IntGen): int =
  var started = false
  while not started or result in g.skip:
    started = true
    result = randomIn(g.rng, g.min, g.max)

# ok
# fix the generator

