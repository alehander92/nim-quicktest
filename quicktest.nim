import macros, breeze, unittest
import strutils, sequtils, strformat, tables, intsets, sets, future, json

type
  QuickRNG* {.inheritable.} = ref object
    seed*: int64

method randomize*(rng: QuickRNG, seed: int64) {.base.} =
  raise newException(ValueError, "not implemented")

method randomInt*(rng: QuickRNG, max: int): int {.base.} =
  raise newException(ValueError, "not implemented")


when not defined(js):
  import random, os, ospaths, marshal, times, ospaths, hashes

  type
    # based on xorshiro128+
    DefaultRNG = ref object of QuickRNG


  method randomInt*(a: DefaultRNG, max: int): int =
    rand(max)


  method randomize*(a: DefaultRNG, seed: int64) =
    randomize(seed)


  # directly taken from random.nim, improve
  proc genSeed: int64 =
    let now = getTime()
    convert(Seconds, Nanoseconds, now.toUnix) + now.nanosecond


  var defaultRNG* = DefaultRNG()



else:
  import jsffi, js_lib

  type
    DefaultRNG = ref object of QuickRNG
      engine*: js
  # seed*: int64

  proc randomInt*(a: DefaultRNG, max: int): int =
    cast[int](a.engine.integer(0, max - 1))

  var require* {.importc.}: (cstring) -> js
  var jsrandom = require(cstring"random-js")
  var console* {.importc.}: js
  proc a*: js {.importcpp: "function(){var random = require('random-js');return new random(random.engines.mt19937().autoSeed())}()".}
  var defaultRNG* = DefaultRNG(engine: a())
  proc now: int64 {.importcpp: "(new Date()).getTime()".}

  proc genSeed: int64 =
    now()


var globalRNG*: QuickRNG


proc registerRNG*(rng: QuickRNG, seed: int64 = genSeed()) =
  globalRNG = rng
  globalRNG.seed = seed

const EMPTY_RANGE = 0 .. -1
var failFastOption* = false
var saveOption* = ""
var reprOption* = ""
when not defined(js):
  var seedOption* = getEnv("QUICKTEST_SEED", "0").parseBiggestInt.int64
  var testOption* = getEnv("QUICKTEST_TEST", "")
  var iterationOption* = getEnv("QUICKTEST_ITERATION", "-1").parseInt
else:
  var seedOption* = 0.int64
  var testOption* = ""
  var iterationOption* = -1

proc init =
  if paramCount() > 0:
    for z in 1 .. paramCount():
      if paramStr(z) == "--fail-fast" or paramStr(z) == "-f":
        failFastOption = true
      elif paramStr(z).startsWith("save:"):
        saveOption = paramStr(z).split(':', 1)[1]
      elif paramStr(z).startsWith("repr:"):
        reprOption = paramStr(z).split(':', 1)[1]
      elif paramStr(z).startsWith("seed:"):
        seedOption = paramStr(z).split(':', 1)[1].parseInt.int64
      elif paramStr(z).startsWith("test:"):
        testOption = paramStr(z).split(':', 1)[1]
        if testOption.len >= 2 and testOption[0] == '"' and testOption[
            ^1] == '"':
          testOption = testOption[1 .. ^2]
      elif paramStr(z).startsWith("iteration:"):
        iterationOption = paramStr(z).split(':', 1)[1].parseInt

  if iterationOption != -1 and testOption.len == 0:
    echo "iteration requirest test name"
    quit(1)
  echo saveOption

when declared(disableParamFiltering):
  disableParamFiltering()

init()

if seedOption == 0'i64:
  registerRNG(defaultRNG)
else:
  registerRNG(defaultRNG, seed = seedOption)

macro genTo(t: untyped): untyped =
  let to = ident"to"
  let i = ident"i"
  let other = ident"other"
  result = quote:
    proc `to*`(`i`: int, `other`: type `t`): `t` =
      `t`(`i`)
# echo result.repr


genTo(int)
genTo(int8)
genTo(int16)
genTo(int32)
genTo(int64)
genTo(uint)
genTo(uint8)
genTo(uint16)
genTo(uint32)
genTo(uint64)

proc generateQuicktest*(args: NimNode): NimNode
proc nameTest*(args: NimNode): string
proc generateTweak(expression: NimNode, ident: NimNode, depth: int = 0,
    analyze: bool = true): (NimNode, Table[string, Table[string, string]])
proc generateReseed(args: NimNode): NimNode


macro quicktest*(args: varargs[untyped]): untyped =
  let code = generateQuicktest(args)
  let reseed = generateReseed(args)
  var name = newLit(nameTest(args))

  result = quote:
    test `name`:
      `reseed`
      `code`

# echo result.repr

proc generateReseed(args: NimNode): NimNode =
  var spec: string
  if args[0].kind != nnkStrLit:
    spec = args[0][1][3].repr
  else:
    spec = args[1][1][3].repr
  # echo spec.hash
  when not defined(js):
    let specLen = newLit(spec.hash)
  else:
    let specLen = newLit(spec.len)
  result = quote:
    globalRNG.randomize(globalRNG.seed + `specLen`)


proc replaceNames(node: var NimNode, names: seq[string]) =
  var z = 0
  for s in node:
    var son = node[z]
    if son.kind in {nnkSym, nnkIdent}:
      if $son in names:
        node[z] = ident("$1Gen" % $son)
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
    t = nnkExprEqExpr.newTree(ident"element", t)
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

# proc generateTypeArgs2(expression: NimNode): seq[NimNode] =
#   var element = expression
#   if element.kind == nnkIdent:
#     var e = quote:
#       Type[`element Gen`]()
#     result = @[e]
#   else:
#     element = element[0]
#     # echo repr(expression)
#     var t = quote:
#       Type(`element`)
#     t = nnkExprEqExpr.newTree(ident"element", t)
#     result = @[t]
#     var z = 0
#     for x in expression:
#       if z > 0:
#         t[1].add(x)
#       inc z

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

macro analyzeObject(name: typed, gen: string,
    fields: varargs[string]): untyped =
  var internal = analyzeInternal(name, fields)
  result = nnkStmtList.newTree()
  for field in internal:
    var v = getType(field)
    var (n, _) = generateTweak(
      nnkExprEqExpr.newTree(
        ident(field.repr),
        nnkCall.newTree(ident(v.repr))),
      ident("$1Field$2" % [$gen, repr(field)]),
      analyze = false)
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
    # var v = getType(field)
    var genQuote = ident($gen)
    var f = ident(field.repr)
    var generator = ident("$1Field$2Gen" % [$gen, repr(field)])
    var t = quote:
      `genQuote`.`f` = `generator`.generate()
    result.add(t)
# echo repr(result)

let BUILTIN_NAMES {.compileTime.} = toSet(["string", "int", "int8", "int16", "int32", "int64",
    "uint", "uint8", "uint16", "uint32", "uint64", "float", "bool"])

proc generateTweak(expression: NimNode, ident: NimNode, depth: int = 0,
    analyze: bool = true): (NimNode, Table[string, Table[string, string]]) =
  var identGen = ident("$1Gen" % $(ident))
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
  # var otherFields: seq[NimNode] = @[]

  # echo "isType", isType
  # echo "isBuiltin", isBuiltin
  # echo "expression", repr(expression)
  if isType and generatorName != "Type":
    if isBuiltin:
      args = concat(@[ident($generatorName)], args)
    else:
      result[1][generatorName] = initTable[string, string]()
      for arg in args:
        # echo repeat("  ", depth), "hm", repr(arg[1])
        # echo repeat("  ", depth), "hm", repr(newIdentNode(!("a")))
        var (argNode, _) = generateTweak(arg, ident("$1Field$2" % [$ident,
            repr(arg[0])]), depth + 1, analyze = analyze)
        newArgs.add(argNode)
        # newArgs.add(generateTweak(arg, ident"a"), depth + 1))
        # echo repeat("  ", depth), "mh", treerepr(newArgs[^1][0][^1])
        if len(newArgs[^1]) > 1:
          newArgs[^1] = newArgs[^1][1]
          # echo result[1][generatorName]
          assert result[1].hasKey(generatorName)
          result[1][generatorName][repr(arg[0])] = "$1Field$2Gen" % [$ident,
              repr(arg[0])]
          newArgs[^1][0][0][0] = ident(result[1][generatorName][repr(arg[0])])

      args = @[ident($generatorName)]
      var generatorNameQuote = ident(generatorName)
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
  var generatorNameNode = ident(generatorName)
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

proc generateGenerator(generator: NimNode, names: seq[string]): (NimNode,
    NimNode, NimNode) =
  assert generator.kind == nnkIdentDefs
  var ident = generator[0]
  var identGen = ident("$1Gen" % $ident)
  var expression = generator[1]
  if generator[1].kind == nnkPrefix and $generator[1][0] == "!":
    if generator[1][1].kind == nnkCall:
      expression = nnkCall.newTree(nnkPrefix.newTree(generator[1][0],
          generator[1][1][0]))
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
      var labelQuote = ident(label)
      var x0 = quote:
        var `ident` = `labelQuote`()
      result[1].add(x0)
      var identString = newLit($ident)
      var t = quote:
        analyzeLoop(`labelQuote`, `identString`)
      for field, fieldGen in fields:
        var fieldQuote = ident(field)
        var fieldGenQuote = ident(fieldGen)
        var x1 = quote:
          `ident`.`fieldQuote` = `fieldGenQuote`.generate()
        result[1].add(x1)
        t.add(newLit(field))
      result[1].add(t)
  var s = newLit($generator[0])
  var error = ident("error")
  var add = ident("add")
  result[2] = quote:
    `error`.`add`("[" & `s` & "] " & $(`ident`) & "\n")

proc serialize*[T](value: T, name: string): JsonNode =
  %(@[name, $$value])

proc serializeTest*(sourcePath: string, success: bool, nodes: varargs[
    JsonNode]) =
  let serialized = %*
    {
      "args": nodes,
      "success": success
    }
  let folder = saveOption
  let name = paramStr(0).rsplit("/", 1)[1].rsplit(".", 1)[0] # TODO js
  createDir(folder / name)
  var max = -1
  for kind, path in walkDir(folder / name, relative = true):
    if path.startsWith("repr_"):
      let id = path[5..^1].split('.', 1)[0].parseInt
      if max < id:
        max = id

  when not defined(js):
    writeFile(folder / name / &"repr_{max + 1}.json", $serialized)
  else:
    js_lib.writeFile(folder / name / &"repr_{max + 1}.json", $serialized)

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
  elif node.kind == nnkPrefix:
    ident(&"{node[1][0]}Obj")
  else:
    # echo node.repr
    node

proc expand(args: NimNode): NimNode =
  result = nnkArgList.newTree()
  for arg in args:
    if arg.kind == nnkEmpty:
      continue
    expectKind arg, nnkIdentDefs
    if arg.len == 3:
      result.add(arg)
    else:
      for z in 0 ..< arg.len - 2:
        result.add(nnkIdentDefs.newTree(arg[z], arg[^2], arg[^1]))


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
  let generators = expand(doNode[3])
  # let generators = toSeq(argsNode).filterIt(it.kind != nnkEmpty)
  let generatorNames = generators.mapIt($it[0])
  let gens = generators.mapIt(generateGenerator(it, generatorNames))
  result = buildMacro:
    stmtList()
  result = result[0]
  var init = buildMacro:
    stmtList()
  init = init[0]
  let error = ident("error")
  var checkpoint = quote:
    var `error` = "\n"

  checkpoint = nnkStmtList.newTree(checkpoint)
  for gen in gens:
    result.add(gen[0])
    init.add(gen[1])
    checkpoint.add(gen[2])


  let test = doNode[^1]
  let index = quote: index
  checkpoint.add quote do: `error`.add("seed:" & $globalRNG.seed & " test:" & 
      `label` & " iteration:" & $`index`)

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
    let path = reprOption
    let serialized = readFile(path)
    let args {.inject.} = serialized.parseJson{"args"}
    `deserializations`
    # `checkpoint`
    `test`

  # TODO: currentSourcePath always quicktest
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
    for `index` in 0..<`times`:
      `init`
      if iterationOption != -1 and iterationOption != `index` and testOption == `label`:
        continue
      `test`
      case testStatusIMPL:
      of FAILED:
        `checkpoint`
        echo `error`
        if saveOption.len > 0:
          `serialize`
        if failFastOption:
          break
      of OK:
        if saveOption.len > 0:
          if successCount mod 20 == 0:
            `serialize`
            successCount = 0
          successCount += 1
      else:
        discard
  let generatedTest = quote:
    if paramCount() > 0 and paramStr(1).startsWith("repr:"):
      `reprCode`
    elif testOption.len > 0 and testOption != `label`:
      skip
    else:
      `generatedElse`
  result.add(generatedTest)
# echo result.repr

# in the future we might have specific rng for a test, no need for now

type
  Alphabet* = enum AUndefined, AAll, AAscii, ALatin, ALatinDigit, ANone

  Arbitrary*[T] = concept a
    arbitrary(type a) is Gen[T]

  Gen*[T] {.inheritable.} = ref object
    last*: T
    fil*: FilterMixin[T]

  LimitMixin*[T] = ref object
    min*: T
    max*: T

  FilterMixin*[T] = ref object
    test*: (T) -> bool
    trans*: (T) -> T

  StringGen* = ref object of Gen[string]
    limit*: LimitMixin[int]
    symbols: seq[char]
    alphabet: Alphabet

  BoolGen* = ref object of Gen[bool]

  CharGen* = ref object of Gen[char]

  FloatGen* = ref object of Gen[float]
    limit*: LimitMixin[float]

  SeqGen*[T] = ref object of Gen[seq[T]]
    # slimit*:    LimitMixin[int]
    # sfil*:      FilterMixin[seq[T]]
    limit*: LimitMixin[int]
    # last*:      seq[T]
    element*: Gen[T]
    infer*: bool

  InsideGen* = ref object of Gen[string]
    limit*: LimitMixin[int]
    inside*: StringGen

  IntGen* = ref object of Gen[int]
    limit*: LimitMixin[int]
    skip*: IntSet

  NumberGen*[T] = ref object of Gen[T]
    limit*: LimitMixin[T]

  SomeNumber = concept a, type T
    a.int is int
    int.to(T) is type(a)


# proc to2[T: SomeNumber](i: int, other: type T): T =
#   i.to(other)


type
  # Iterator[T] = concept a
  #   for element in a:
  #     element is T

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

proc arbitrary*[T: SomeNumber](t: typedesc[T]): NumberGen[T] =
  NumberGen[T]()

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

proc arbitrary*[T](t: typedesc[NumberGen[T]]): NumberGen[T] =
  NumberGen[T]()

proc randomIn*(rng: QuickRNG, min: int, max: int): int =
  if min == int.low:
    return rng.randomInt(int.high)
  return rng.randomInt(max - min) + min

# proc randomUInt*[T: uint | uint16 | uint32 | uint64](rng: var Engine, min: T, max: T): T =
#   return randomUInt(rng, max - min) + min

# TODO: uint rng
proc randomIn*[T](rng: QuickRNG, min: T, max: T, t: type T): T =
  if min == max:
    return max
  # return min + randomInt(rng, t) mod (max - min)
  return rng.randomIn(min.int, max.int).to(T)

proc choice*[T](rng: QuickRNG, elements: seq[T]): T =
  result = elements[rng.randomInt(elements.len)]

proc toSeq*[T](s: set[T]): seq[T] =
  result = @[]
  for c in s:
    result.add(c)

proc generateSequence*[T](rng: QuickRNG, elements: seq[T], min: int,
    max: int): seq[T] =
  result = @[]
  var length = randomIn(rng, min, max)
  for element in 0..<length:
    result.add(choice(rng, elements))

proc generateSequence*[T](rng: QuickRNG, generator: (int) -> T, limit: int,
    min: int, max: int): seq[T] =
  result = @[]
  var length = randomIn(rng, min, max)
  for element in 0..<length:
    var number = rng.randomInt(limit)
    result.add(generator(number))


proc loadRange(min: int, max: int, range: Slice[int]): (int, int) =
  var minResult = min
  var maxResult = max
  if range.a != EMPTY_RANGE.a or range.b != EMPTY_RANGE.b:
    minResult = range.a
    maxResult = range.b
  return (minResult, maxResult)

proc loadRange[T: SomeNumber](min: T, max: T, range: Slice[T]): (T, T) =
  # echo min, " ", max, " ", range
  var minResult = min
  var maxResult = max
  if range.a.int != EMPTY_RANGE.a or range.b.int != EMPTY_RANGE.b:
    minResult = range.a
    maxResult = range.b
  return (minResult, maxResult)



# why do we need those if we have Type
proc String*(
    symbols: set[char],
    test: (string) -> bool = nil,
    trans: (string) -> string = nil,
    min: int = 0,
    max: int = 20,
    range: Slice[int] = EMPTY_RANGE): StringGen =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = StringGen(
    limit: LimitMixin[int](min: minArg, max: maxArg),
    fil: FilterMixin[string](test: test, trans: trans),
    symbols: toSeq(symbols),
    alphabet: AUndefined,
    last: "")


proc String*(
    alphabet: Alphabet = AAll,
    min: int = 0,
    max: int = 20,
    range: Slice[int] = EMPTY_RANGE): StringGen =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = StringGen(
    limit: LimitMixin[int](min: minArg, max: maxArg),
    fil: FilterMixin[string](test: nil, trans: nil),
    alphabet: alphabet,
    last: "")



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
    range: Slice[int] = EMPTY_RANGE,
    alphabet: Alphabet = AUndefined): StringGen =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = StringGen(
    limit: LimitMixin[int](min: minArg, max: maxArg),
    fil: FilterMixin[string](test: test, trans: trans),
    alphabet: alphabet,
    last: "")


proc Type*(
    t: typedesc[int],
    test: (int) -> bool = nil,
    trans: (int) -> int = nil,
    min: int = 0,
    max: int = 20,
    range: Slice[int] = EMPTY_RANGE): IntGen =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = IntGen(
    limit: LimitMixin[int](min: minArg, max: maxArg),
    fil: FilterMixin[int](test: test, trans: trans),
    skip: uniq(@[]))


proc Type*(
    t: typedesc[bool],
    test: (bool) -> bool = nil,
    trans: (bool) -> bool = nil): BoolGen =

  result = BoolGen(fil: FilterMixin[bool](test: test, trans: trans))


proc Type*(
    t: typedesc[char],
    test: (char) -> bool = nil,
    trans: (char) -> char = nil): CharGen =

  result = CharGen(fil: FilterMixin[char](test: test, trans: trans))


# range for float is not a good fit
proc Type*(
    t: typedesc[float],
    test: (float) -> bool = nil,
    trans: (float) -> float = nil,
    min: float = 0.0,
    max: float = 20.0): FloatGen =

  result = FloatGen(
    limit: LimitMixin[float](min: min, max: max),
    fil: FilterMixin[float](test: test, trans: trans))


proc Type*[T](
    t: typedesc[seq[T]],
    test: (seq[T]) -> bool = nil,
    trans: (seq[T]) -> seq[T] = nil,
    min: int = 0,
    max: int = 20,
    range: Slice[int] = EMPTY_RANGE): SeqGen[T] =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = SeqGen[T](
    element: arbitrary(T),
    infer: true,
    limit: LimitMixin[int](min: minArg, max: maxArg),
    fil: FilterMixin[seq[T]](test: test, trans: trans))


proc Type*[T](
    t: typedesc[seq[T]],
    element: Arbitrary[T],
    test: (seq[T]) -> bool = nil,
    trans: (seq[T]) -> seq[T] = nil,
    min: int = 0,
    max: int = 20,
    range: Slice[int] = EMPTY_RANGE): SeqGen[T] =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = SeqGen[T](
    element: element,
    infer: false,
    limit: LimitMixin[int](min: minArg, max: maxArg),
    fil: FilterMixin[seq[T]](test: test, trans: trans))


proc Type*[T: SomeNumber](
    t: typedesc[T],
    # test: (T) -> bool = nil,
    # trans: (T) -> int = nil,
  min: T = 0,
    max: T = 20,
    range: Slice[int] = EMPTY_RANGE): NumberGen[T] =

  let tRange = HSlice[T, T](a: range.a.to(T), b: range.b.to(T))
  let (minArg, maxArg) = loadRange(min, max, tRange)
  result = NumberGen[T](
    limit: LimitMixin[T](min: minArg, max: maxArg),
    fil: FilterMixin[T]())


# proc Type*[T](
#     t: typedesc[T]): Gen[T] =
#   result = Gen[T](rng: initRng())


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

  chars = generateSequence(globalRNG, generator, limit, g.min, g.max)
  result = chars.join()

proc Inside*(s: StringGen, min: int = 0, max: int = 10): InsideGen =
  result = InsideGen(limit: LimitMixin[int](min: min, max: max), inside: s)

proc generateInternal*(g: var InsideGen): string =
  var length = randomIn(globalRNG, g.min, min(g.max, max(g.min + 1,
      len(g.inside.last))))
  var first = randomIn(globalRNG, 0, max(1, len(g.inside.last) - length))
  result = g.inside.last[first..first + length - 1]
  if len(result) < g.min:
    result.add(repeat(" ", g.min - len(result)))


proc Int*(
    min: int = low(int),
    max: int = high(int),
    range: Slice[int] = EMPTY_RANGE,
    skip: seq[int] = @[]): IntGen =

  let (minArg, maxArg) = loadRange(min, max, range)
  result = IntGen(
    limit: LimitMixin[int](min: minArg, max: maxArg),
    skip: uniq(skip))


proc generateInternal*(g: var IntGen): int =
  var started = false
  # echo g.min, " ", g.max
  while not started or result in g.skip:
    started = true
    result = randomIn(globalRNG, g.min, g.max)

proc generateInternal*(g: var BoolGen): bool =
  result = bool(randomIn(globalRNG, 0, 1))

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
  result = chr(randomIn(globalRNG, 0, 127))

proc generateInternal*(g: var FloatGen): float =
  result = float(randomIn(globalRNG, int(g.min), int(g.max)))


proc generateInternal*[T](g: var SeqGen[T]): seq[T] =
  var length = randomIn(globalRNG, g.limit.min, g.limit.max)
  result = @[]
  for z in 0..<length:
    if g.infer:
      var t = arbitrary(T)
      result.add(t.generate())
    else:
      var t = cast[type(arbitrary(T))](g.element) # are you fucking kidding me
      result.add(t.generate())


proc generateInternal*[T: SomeNumber](g: var NumberGen[T]): T =
  # echo "value", g.limit.min, g.limit.max is uint
  when T is int:
    result = randomIn(globalRNG, g.limit.min.int, g.limit.max.int).to(T)
  else:
    result = randomIn(globalRNG, g.limit.min, g.limit.max, T)

# ok
# fix the generator

export json
when defined(js):
  export js_lib


