# nim-quickcheck

A quickcheck-related and hypothesis-inspired library extremely early phase. (as it's kinda POC)

```nim
quicktest "reverse" do(s: string(symbols={'a'..'z'}, min=0, max=20)):
  check(s.reverse().reverse() == s)
```


```nim
  quicktest "mapIt" do (s: seq[int(min = 0, max = 20)]):
    check(s.mapIt(b(it)).mapIt(c(it)) == s)
```

```nim
  quicktest "object" do (s: MyObject(a = string(alphabet=ALatin, max = 20))):
    check(s.name().split('_')[^1] == s.a) # we generate random data for the other fields too
    # however automated object fields support is still very early
```

I am trying to balance the power of types with extreme flexibility in tweaking 
generation for them, combining the haskell quickcheck-like heavy type approach with 
the way more generator-based hypothesis one.

Basically most of the generation should be automatically composed based on compile time
type introspection, but you should be able to tweak options for each element of the type

## how it works

Obviously it's all macros expanding other macros and stuff

You can reuse builtin type names most of the type and also refine them with some filters, e.g.
`string(alphabet = ALatin)` generates strings with latin letters, `seq[int(max = 2)]` generates seq-s with values max 2

It should be possible to compose those easily (and eventually I might add the posibility to define your own custom filters)

Better documentation coming

## custom generation

You can also define your own custom generators and invoke them:

```nim
  quicktest("analyze", 1) do(node: !Node(filename)):
    check(..)
```

Here Node can be a custom generator that generates a valid AST node from the parsed filename each time just as an example (I use more complicated generators based on real data as files and directories all the time)

Currently the custom generators aren't extremely simple to write, but kinda:

```nim
NodeGen* = ref object of Gen[Node]
  filename*: string
  ast*: AST

proc Node*(filename: string): NodeGen =
  result = NodeGen(filename: filename, ast: parse(filename))

proc generate*(n: var NodeGen): Node =
  result = deepChoice(n.ast)
```

You basically define your generator, its constructor and the generate function.
I'll probably write a macro for easier definitions

## how to run

take a look at [base.nim](base.nim)

```bash
nim c base.nim
./base
```

quicktest is made to play well with unittest (I've talked with zah about it)

Currently errors are reported for each failing example. It's easy to customize,
I've once implemented automated tracker issue creation from failing tests.

Eventually I'll add the feature to save all failing tests as normal test examples inspired by hypothesis.

## how stable

I use it for my projects and it works well for them, but I know the limitations, so it's probably very
unstable still for other people. Look at it as alpha software.

## well

I'd be glad for any kind of ideas, criticism or contribution. 

