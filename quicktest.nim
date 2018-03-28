import macros, breeze, unittest
import strutils, sequtils, strformat, tables, intsets, sets, future, json

when not defined(js):
  import random.urandom, random.xorshift, os, ospaths, marshal

  type
    Engine = SystemRandom
  var engine*: Engine = initSystemRandom()
  proc initRng: Engine =
    engine

  # hm
else:
  import jsffi, js_lib

  type
    Engine* = ref object of js
      engine*: js

  proc randomInt*(a: var Engine, max: int): int =
    cast[int](a.engine.integer(0, max - 1))

  var require* {.importc.}: (cstring) -> js
  var jsrandom = require(cstring"random-js")
  var console* {.importc.}: js
  proc a*: js {.importcpp: "function(){var random = require('random-js');return new random(random.engines.mt19937().autoSeed())}()".}
  var engine*: Engine = Engine(engine: a())

  proc initRng: Engine =
    engine


proc generateQuicktest*(args: NimNode): NimNode

proc nameTest*(args: NimNode): string

proc generateTweak(expression: NimNode, ident: NimNode, depth: int = 0, analyze: bool = true): (NimNode, Table[string, Table[string, string]])

macro ObjectGen*(t: typed, args: untyped): untyped =
  var typ = getType(t)
  if typ.kind == nnkBracketExpr and typ[1].kind == nnkBracketExpr and $typ[1][0] == "ref":
    typ = getType(typ[1][1])
  var name = args[0]
  var call = args[1][0]
  result = quote:
    echo 0


macro quicktest*(args: varargs[untyped]): untyped =
  result = generateQuicktest(args)
  var name = newLit(nameTest(args))
  result = quote:
    test `name`:
      `result`
  # echo repr(result)

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

proc generateTypeArgs(expression: NimNode, base: NimNode): seq[NimNode] =
  var element = expression
  if element.kind == nnkIdent:
    var e = quote:
      Type[`element Gen`]()
    result = @[e]
  else:
    element = element[0]
    var t = quote:
      Type(`element`)
    t = nnkExprEqExpr.newTree(newIdentNode(!"element"), t)
    result = @[t]
    var z = 0
    for x in expression:
      if z > 0:
        t[1].add(x)
      inc z        
  var a = base
  var t2 = quote:
    `a`[`element`]
  result = concat(@[t2], result)

proc generateTypeArgs2(expression: NimNode): seq[NimNode] =
  var element = expression
  if element.kind == nnkIdent:
    var e = quote:
      Type[`element Gen`]()
    result = @[e]
  else:
    element = element[0]
    # echo repr(expression)
    var t = quote:
      Type(`element`)
    t = nnkExprEqExpr.newTree(newIdentNode(!"element"), t)
    result = @[t]
    var z = 0
    for x in expression:
      if z > 0:
        t[1].add(x)
      inc z        

proc analyzeInternal(name: NimNode, fields: NimNode): seq[NimNode] =
  var x = getType(name)
  var y = getType(x[1][1])
  # echo treerepr(y)
  var fieldsRepr = toSet[string](@[])
  for field in fields:
    fieldsRepr.incl(repr(field)[1..^2])
  result = @[]
  for field in y[2]:
    if repr(field) notin fieldsRepr:
      result.add(field)

macro analyzeObject(name: typed, gen: string, fields: varargs[string]): untyped =
  var internal = analyzeInternal(name, fields)
  result = nnkStmtList.newTree()
  for field in internal:
    var v = getType(field)
    var (n, t) = generateTweak(
      nnkExprEqExpr.newTree(
        newIdentNode(!repr(field)),
        nnkCall.newTree(newIdentNode(!repr(v)))),
      newIdentNode(!("$1Field$2" % [$gen, repr(field)])),
      analyze=false)
    # echo treerepr(n)
    result.add(nnkVarSection.newTree(
      nnkIdentDefs.newTree(
        n[0][0][0],
        newEmptyNode(),
        n[1][0][0][2])))

macro analyzeLoop(name: typed, gen: string, fields: varargs[string]): untyped =
  var internal = analyzeInternal(name, fields)
  result = nnkStmtList.newTree()
  for field in internal:
    var v = getType(field)
    var genQuote = newIdentNode(!($gen))
    var f = newIdentNode(!repr(field))
    var generator = newIdentNode(!("$1Field$2Gen" % [$gen, repr(field)]))
    var t = quote:
      `genQuote`.`f` = `generator`.generate()
    result.add(t)
  # echo repr(result)

let BUILTIN_NAMES = toSet(["string", "int", "float", "bool"])

proc generateTweak(expression: NimNode, ident: NimNode, depth: int = 0, analyze: bool = true): (NimNode, Table[string, Table[string, string]]) =
  var identGen = newIdentNode(!("$1Gen" % $(ident)))
  var generatorName: string
  var args: seq[NimNode] = @[]
  var isBuiltin = true
  var isType = true
  if expression.kind == nnkIdent:
    generatorName = $expression
  elif expression.kind == nnkPrefix and $expression[0] == "!":
    generatorName = $expression[1]
    isBuiltin = false
    isType = false
  else:
    if expression[0].kind == nnkPrefix and $expression[0][0] == "!":
      generatorName = $(expression[0][1])
      isBuiltin = false
      isType = false
    elif expression.kind == nnkBracketExpr and $expression[0] == "seq":
      generatorName = "Type"
      args = generateTypeArgs(expression[1], expression[0])
    else:
      generatorName = $(expression[0])
      if generatorName notin BUILTIN_NAMES:
        isBuiltin = false
    if len(args) == 0:
      var z = 0
      for arg in expression:
        if z > 0:
          args.add(arg)
        inc z
  result[1] = initTable[string, Table[string, string]]()
  var newArgs: seq[NimNode] = @[]
  var otherFields: seq[NimNode] = @[]

  # echo "isType", isType
  # echo "isBuiltin", isBuiltin
  # echo "expression", repr(expression)
  if isType and generatorName != "Type":
    if isBuiltin:
      args = concat(@[newIdentNode(!($generatorName))], args)
    else:
      result[1][generatorName] = initTable[string, string]()
      for arg in args:
        # echo repeat("  ", depth), "hm", repr(arg[1])
        # echo repeat("  ", depth), "hm", repr(newIdentNode(!("a")))
        # echo repeat("  ", depth), "hm", treerepr(arg[1])
        var (argNode, argTable) = generateTweak(arg, newIdentNode(!("$1Field$2" % [$ident, repr(arg[0])])), depth + 1, analyze=analyze)
        newArgs.add(argNode)
        # newArgs.add(generateTweak(arg, newIdentNode(!("a")), depth + 1))
        # echo repeat("  ", depth), "mh", treerepr(newArgs[^1][0][^1])
        if len(newArgs[^1]) > 1:
          newArgs[^1] = newArgs[^1][1]
          # echo result[1][generatorName]
          assert result[1].hasKey(generatorName)
          result[1][generatorName][repr(arg[0])] = "$1Field$2Gen" % [$ident, repr(arg[0])]
          newArgs[^1][0][0][0] = newIdentNode(!(result[1][generatorName][repr(arg[0])]))

      args = @[newIdentNode(!($generatorName))]
      # args = @[generateTweak(args[0], newIdentNode(!($generatorName)))]
      # args = concat(@[newIdentNode(!($generatorName))], generateTypeArgs())
      var generatorNameQuote = newIdentNode(!generatorName)
      var identQuote = newLit($ident)
      if analyze:
        var t = quote:
          analyzeObject(`generatorNameQuote`, `identQuote`)
        for label, field in result[1][generatorName]:
          t.add(newLit(label))
        newArgs.add(t)
      # var t: Table[string, string]
      # (otherFields, t) = analyzeObject(generatorName, result[1][generatorName])
      # result[1][generatorName] = t
    generatorName = "Type"
  var generatorNameNode = newIdentNode(!generatorName)
  # if generator[1].kind == nnkObjConstr:
  #   result[0] = quote:
  #     var `identGen` = ObjectGen()
  # else:
  result[0] = quote:
    var `identGen` = `generatorNameNode`()
  for arg in args:
    result[0][0][2].add(arg)
  result[0] = nnkStmtList.newTree(result[0])
  for newArg in newArgs:
    result[0].add(newArg)
  
proc generateGenerator(generator: NimNode, names: seq[string]): (NimNode, NimNode, NimNode) =
  assert generator.kind == nnkIdentDefs
  var ident = generator[0]
  var identGen = newIdentNode(!("$1Gen" % $(ident)))
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

  var fieldTable: Table[string, Table[string, string]]
  (result[0], fieldTable) = generateTweak(expression, ident)

  if len(fieldTable) == 0:
    result[1] = quote:
      var `ident` = `identGen`.generate()
      `identGen`.setLast(`ident`)
  else:
    result[1] = nnkStmtList.newTree()
    for label, fields in fieldTable:
      var labelQuote = newIdentNode(!label)
      var x0 = quote:
        var `ident` = `labelQuote`()
      result[1].add(x0)
      var identString = newLit($ident)
      var t = quote:
        analyzeLoop(`labelQuote`, `identString`)
      for field, fieldGen in fields:
        var fieldQuote = newIdentNode(!field)
        var fieldGenQuote = newIdentNode(!fieldGen)
        var x1 = quote:
          `ident`.`fieldQuote` = `fieldGenQuote`.generate()
        result[1].add(x1)
        t.add(newLit(field))
      result[1].add(t)
  var s = newLit($generator[0])
  var error = newIdentNode(!"error")
  var add = newIdentNode(!"add")
  result[2] = quote:
    `error`.`add`("[" & `s` & "] " & $(`ident`) & "\n")

proc serialize*[T](value: T, name: string): JsonNode =
  %(@[name, $$value])

proc serializeTest*(sourcePath: string, success: bool, nodes: varargs[JsonNode]) =
  let serialized = %*
    {
      "args": nodes,
      "success": success
    }
  let folder = paramStr(1).split(':', 1)[1]
  let name = sourcePath.rsplit("/", 1)[1].rsplit(".", 1)[0] # TODO js
  createDir(folder / name)
  var max = -1
  for kind, path in walkDir(folder / name, relative=true):
    if path.startsWith("repr_"):
      let id = path[5..^1].split('.', 1)[0].parseInt
      if max < id:
        max = id

  writeFile(folder / name / &"repr_{max + 1}.json", $serialized)

proc nameTest*(args: NimNode): string =
  if args[0].kind == nnkStrLit:
    return $(args[0])
  else:
    return $(args[0][0])
  
proc toTypename*(node: NimNode): NimNode =
  if node.kind == nnkIdent:
    node
  elif node.kind == nnkCall:
    node[0]
  else:
    echo node.repr
    node

proc generateQuicktest*(args: NimNode): NimNode =
  var label: string
  var times = newLit(50)
  var doNode: NimNode
  if args[0].kind == nnkStrLit:
    assert args[1].kind == nnkCall
    label = $(args[0])
    times = args[1][0]
    doNode = args[1][1]
  else:
    assert args[0].kind == nnkCall
    label = $(args[0][0])
    doNode = args[0][1]
  let generators = toSeq(doNode[3]).filterIt(it.kind != nnkEmpty)
  # echo treerepr(generators[0])
  # echo treerepr(generators[1])
  let generatorNames = generators.mapIt($it[0])
  let gens = generators.mapIt(generateGenerator(it, generatorNames))
  result = buildMacro:
    stmtList()
  result = result[0]
  var init = buildMacro:
    stmtList()
  init = init[0]
  let error = newIdentNode(!"error")
  var checkpoint = quote:
    var `error` = "values:\n"

  checkpoint = nnkStmtList.newTree(checkpoint)
  for gen in gens:
    result.add(gen[0])
    init.add(gen[1])
    checkpoint.add(gen[2])
  
  let test = doNode[^1]
  let acheckpoint = newIdentNode(!"checkpoint")
  let e = quote:
    `acheckpoint`(`error`)
  checkpoint.add(e)


  var deserializations = nnkStmtList.newTree()
  for z, name in generatorNames:
    let typename = toTypename(generators[z][1])
    let nameNode = ident(name)
    when not defined(js):
      let deserialization = quote:
        var `nameNode` = marshal.to[`typename`](args[`z`][1].getStr())
    else:
      let deserialization = quote:
        var `nameNode` = js_lib.to[`typename`](args[`z`][1].getStr())
    deserializations.add(deserialization)

  let reprCode = quote:
    let path = paramStr(1)[5..^1]
    let serialized = readFile(path)
    let args {.inject.} = serialized.parseJson{"args"}
    `deserializations`
    `checkpoint`
    `test`

  var serialize = nnkCall.newTree(
    ident("serializeTest"),
    ident("currentSourcePath"),
    nnkInfix.newTree(
      ident("=="),
      ident("testStatusIMPL"),
      ident("OK")))
  for name in generatorNames:
    serialize.add(
      nnkCall.newTree(
        ident("serialize"),
        ident(name),
        newLit(name)))

  var generatedElse = quote:
    var successCount = 0
    for z in 0..<`times`:
      `init`
      `checkpoint`
      `test`
      if paramCount() > 0 and paramStr(1).startsWith("save:"):
        case testStatusIMPL:
        of FAILED:
          `serialize`
        of OK:
          if successCount == 0:
            `serialize`
            successCount = 0
          successCount += 1
        else:
          discard
  let generatedTest = quote:
    if paramCount() > 0 and paramStr(1).startsWith("repr:"):
      `reprCode`
    else:
      `generatedElse`
  result.add(generatedTest)
  # echo result.repr

type
  Alphabet* = enum AUndefined, AAll, AAscii, ALatin, ALatinDigit, ANone

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

  WithLimit*[T] = concept a
    a.limit is LimitMixin[T]

  WithFilter*[T] = concept a
    a.fil is FilterMixin[T]

proc setLast*[T](a: Gen[T], value: T) =
  a.last = value

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
  return randomInt(rng, max - min) + min

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

proc Type*[T](
    t: typedesc[T]): Gen[T] =
  result = Gen[T](rng: initRng())

proc generateInternal*(g: var StringGen): string =
  var chars: seq[char]
  # if g.alphabet == AUndefined:
  #   chars = generateSequence(g.rng, g.symbols, g.min, g.max)
  #   result = chars.join()
  # else:
  var generator: (int) -> char
  var limit: int
  case g.alphabet:
  of AAll:
    generator = (number: int) => chr(number)
    limit = 256
  of AAscii:
    generator = (number: int) => chr(number)
    limit = 128
  of ALatin:
    generator = proc (number: int): char =
      if number < 26:
        chr('a'.ord + number)
      else:
        chr('A'.ord + number - 26)
    limit = 52
  of ALatinDigit:
    generator = proc (number: int): char =
      if number < 26:
        chr('a'.ord + number)
      elif number < 52:
        chr('A'.ord + number - 26)
      else:
        chr('0'.ord + number - 52)
    limit = 62
  of ANone, AUndefined:
    generator = (number: int) => chr(number)
    limit = 256
    # raise newException(ValueError, "None")

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
  # while not started or not (g.test() == nil or g.test()(result)):
  while not started or not (g.fil.test == nil or g.fil.test(result)):
    started = true
    result = generateInternal(g)
    if g.fil.trans != nil:
      result = g.fil.trans(result)

proc generate*[T](g: var SeqGen[T]): seq[T] =
  var started = false
  # while not started or not (g.test() == nil or g.test()(result)):
  while not started or not (g.fil.test == nil or g.fil.test(result)):
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
    if g.infer:
      var t = arbitrary(T)
      result.add(t.generate())
    else:
      var t = cast[type(arbitrary(T))](g.element) # are you fucking kidding me
      result.add(t.generate())
    
when declared(disableParamFiltering):
  disableParamFiltering()

# echo Type(int, min = 0, max = 20) is IntGen
# echo arbitrary(typedesc[IntGen]) is Gen[int]
# ok
# fix the generator

export json
when defined(js):
  export js_lib

