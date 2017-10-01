import base64
import unittest, quickcheck, strutils

suite "base":
  # test "encode":
  #   quickcheck(s = Text(max=80)):
  #     echo "[s] $1" % s
  #     check(decode(encode(s)) == s)

  quicktest "encode" do(s: string(min = 0, max = 20), z: int):
    check(decode(encode(s)) == s)
