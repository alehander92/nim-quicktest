import unittest
import quickcheck, strutils, sequtils, tables, intsets

proc invalidSplit(s: string, separator: string): seq[string] = 
  if len(s) == 0:
    result = @[" "]
  else:
    result = s.split(separator)

proc reverse(s: string): string =
  result = ""
  for z in countdown(s.high, s.low):
    result.add(s[z])

type
  M = ref object
    a: int

  MyGen = ref object of Gen[M]
    a*: IntGen

proc a(m: M): int =
  m.a

proc `$`(m: M): string =
  "M($1)" % $m.a

proc generate(m: var MyGen): M =
  result = M(a: m.a.last)
  m.last = result

proc myGen(aGen: IntGen): MyGen =
  result = MyGen(a: aGen)

suite "standart":
  test "split":
    quickcheck(s = Text(min=0, max=20), separator = Inside(s, min=1, max=8)):
      check(s.split(separator).join(separator) == s)

  test "reverse":
    quickcheck(s = Text(symbols={'a'..'z'}, min=0, max=20)):
      check(s.reverse().reverse() == s)

  # test "addition":
  #   quickcheck(left = Int(), right = Int()):
  #     check((left + right) - right == left)

  # test "multiply":
  #   quickcheck(left = Int(), right = qint(skip= @[0])):
  #     check((left * right) div right == left)

  test "my":
    quickcheck(a=Int(max=200), m = myGen(a=a)):
      check(a(m) == a)

  test "normal":
    check(2 == 2)

  echo "ok"

