# nim-quickcheck

A quickcheck-related library in extremely early phase.

```nim
test "reverse":
  quickcheck(s = Text(symbols={'a'..'z'}, min=0, max=20)):
    check(s.reverse().reverse() == s)
```

