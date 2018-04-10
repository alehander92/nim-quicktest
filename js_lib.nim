import jsffi, async, strutils, sequtils, strformat, macros, json

type
  js* = JsObject

  JSONLib = ref object
    stringify: proc(c: js): cstring
    parse: proc(c: cstring): js
  
  WriteDirEffect = object of WriteIOEffect

  ReadDirEffect = object of ReadIOEffect

  FS* = ref object
    readFileSync*: proc(path: cstring, options: js): cstring
    writeFileSync*: proc(path: cstring, raw: cstring, options: js)
    mkdirSync*: proc(path: cstring) {.tags: [WriteDirEffect].}
    readdirSync*: proc(path: cstring): seq[cstring] {.tags: [ReadDirEffect].}
    existsSync*: proc(path: cstring): bool {.tags: [ReadIOEffect].}

  Process = ref object
    argv*: seq[cstring]

  
  Path = ref object
    join*: proc: cstring {.varargs.}

  PathComponent* = enum   ## Enumeration specifying a path component.
    pcFile,               ## path refers to a file
    pcLinkToFile,         ## path refers to a symbolic link to a file
    pcDir,                ## path refers to a directory
    pcLinkToDir           ## path refers to a symbolic link to a directory

var require {.importc.}: proc(lib: cstring): js
var JSON {.importc.}: JSONLib
var fs* = cast[FS](require("fs"))
var process {.importc.}: Process
var path = cast[Path](require("path"))

proc `$$`*[T](e: T): string =
  $JSON.stringify(cast[js](e))

proc to*[T](e: string): T =
  cast[T](JSON.parse(cstring(e)))

proc readFile*(path: string): string =
  $fs.readFileSync(cstring(path), js{encoding: cstring"utf8"})

proc writeFile*(path: string, raw: string) =
  fs.writeFileSync(cstring(path), cstring(raw), js{encoding: cstring"utf8"})

proc createDir*(dir: string) {.tags: [WriteDirEffect, ReadDirEffect, ReadIOEffect].} =
  if not fs.existsSync(cstring(dir)):
    fs.mkdirSync(cstring(dir))

proc paramCount*: int {.tags: [ReadIOEffect].} =
  process.argv.len - 2

proc paramStr*(i: int): TaintedString {.tags: [ReadIOEffect].} =
  $process.argv[i + 1]

iterator walkDir*(path: string, relative: bool = false): tuple[kind: PathComponent, path: string] {.tags: [ReadDirEffect].} =
  let filenames = fs.readdirSync(cstring(path)).mapIt($it)
  for filename in filenames:
    let path = if relative: filename else: path & filename
    # TODO kind relative
    yield (kind: pcFile, path: path)

proc `/`*(left: string, right: string): string =
  $(path.join(cstring(left), cstring(right)))
