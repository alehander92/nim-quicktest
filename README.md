# nim-quicktest

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

## errors

We display the generater args for an error and all errors in a test.
If you pass the `-f / --fail-fast` option we stop on the first error.
On an error you get the seed, test and iteration index
You can pass them again using an ENV variable: if the test has spaces
you can put it in "".


```bash
QUICKTEST_SEED = ..
# optionally:
QUICKTEST_TEST = ..
QUICKTEST_ITERATION = ..
```
or as args

```bash
./atest -f seed:.. test:.. iteration:..
```

If you pass only the seed, quicktest will reproduce all tests, if you pass
test and iteration too, it will only reproduce this iteration. This can break if you use additional
randomness inside the test: even something that depends on globalRNG.
Iteration arg requires test arg.

## overriding random generator

Quicktest uses QuickRNG which has `seed*: int64` and 

```bash
method randomize*(rng: QuickRNG, seed: int64) {.base.} =
  raise newException(ValueError, "not implemented")

method randomInt*(rng: QuickRNG, max: int): int {.base.} =
  raise newException(ValueError, "not implemented")
```

You need to inherit it and override the methods to implement your own generator.

You can `registerRNG(myRNG, seed=my)` and you can `registerRNG(defaultRNG, seed=my)` if
you want to just change the seed: currently it's generated based on time which is not perfect

## options

```bash
save:folder # saves in folder subfolder with json reproductions
repr:path # reproduces json save
-f / --fail-fast # fails after first error
--seed # initial seed
--test # test to reproduce
--iteration # iteration to reproduce
```

## how does it work

Obviously it's all macros expanding other macros and weird stuff.

You can reuse builtin type names most of the type and also refine them with some filters, e.g.
`string(alphabet = ALatin)` generates strings with latin letters, `seq[int(max = 2)]` generates seq-s with values max 2

It should be possible to compose those easily. You can also define your own matchers.

Better documentation coming.

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

## reproducability

Currently we have initial support for save

If you run `./testname save:testsdir/` we will save each 10th succesfull and each fail arg set in `testsdir/testname/repr_id.json`

Then you can run it again using `./testname repr:testsdir/testname/repr_id.json`

## how stable

I use it for my projects and it works well for them, but I know the limitations, so it's probably very
unstable still for other people. Look at it as alpha software.

## well

I'd be glad for any kind of ideas, criticism or contribution. 

