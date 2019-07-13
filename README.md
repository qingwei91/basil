## Basil

![Build](https://travis-ci.org/qingwei91/basil.svg?branch=master)

A json `Decoder` that can extract data without reading everything from a input json.

The main idea is to describe the data you need from json as recursive data structure called `ParseOps`, which then get interpreted into a `Parse` function which have a signature of `String => YourData` that you can use to parse json, the actual function signature is slightly more complex, but the idea is the same.

**Warning**: This library is an experiment, it is not production ready.

### To try it out

```scala
libraryDependencies += Seq(
    "io.github.qingwei91" %% "basil-core"   % "0.0.7",
    "io.github.qingwei91" %% "basil-derive" % "0.0.7"
)
```

### Features

* Extract data from partial json, it works as long as the part to be extract is valid
* No intermediate json ast, eg. you get `"mystring"` instead of `JString("mystring")`
* Composable parse tree via FreeApplicative, eg. `GetString + GetNum => GetStringAndNum`
* Extract data for case class automatically via `basil-derive` module


### Example

Extract primitive value from json
```scala
import basil.syntax.ParseOpsConstructor._
import basil.parser.implicits._
import basil.parser._

val completeJS = s"""{"key1": "valueA", "key2": 2020.111}"""

val inCompleteJS = s"""{"key1": "valueA", "key2":2020.111, "key3":[200,]}"""


val parseOps = Start.getKey("key2").getNum.eval

val result = Parser.parseJS(parseOps, completeJS.toCharArray.toList).head.map(_._1)

result == 2020.111

```

Extract case class from json
(Note: recursive ADT is not supported yet)
```scala
case class Person(name: String, age: Double)
case class Order(id: String, size: String, belongsTo: Person)

// this imports support deriving parse function for case class
import basil.derive.DeriveParseOps._

val what     = Start.getI[Order].eval
val js       =
    ("id" -> "hoho") ~
    ("size" -> "20") ~
    ("belongsTo" ->
        ("name" -> "Qing") ~
        ("age" -> 20)
    )
val jsString = pretty(render(js))
val res      = Parser.parseString(what, jsString)

res == Success(Order("hoho", "20", Person("Qing", 20)))
```

For more example, check out the test:

[Json Parsing example](./core/src/test/scala/basil/parser/ParseSpec.scala)
[Case class parsing example](./derive/src/test/scala/basil/derive/DeriveParseSpec.scala)

### How to compose ParseOps?

`ParseOps` is a sealed trait, it supports 2 ways of composition by `GetSum` and `GetProduct`, which represent Sum Type and Product Type respectively.

```scala
final case class GetSum[F[_], I](oneOf: NonEmptyMap[String, Lazy[F[I]]]) extends ParseOps[F, I]
```

GetSum expresses that the data we want should be one of the entries on the NonEmptyMap, it expects a `type` field in the json to match with the `key` of the NonEmptyMap to know which path to choose

```scala
final case class GetProduct[F[_], I](allOf: FreeApplicative[F, I]) extends ParseOps[F, I]
```

GetProduct expresses that we want a combination of multiple field, it make uses of `FreeApplicative`, which describes a combination of multiple effects. 

### How it works

The main idea is to define the data you want from the json as a ParseOps, which is a GADT that can be recursive by using `HFix`.

There's some helper in `ParseOpsConstructor` to allow creating a tree that describe the data you need without having to fiddle with `HFix`

```scala
import basil.parser.Parser
import basil.parser.implicits._
import basil.data.ParseOpsConstructor._

val jsString = s"""{ "name" : { "first": "Pika", "last": "chu" } }"""


val getName = Start.getKey("name")

val getFirstName = getName.getKey("first").getString
val getLastName = getName.getKey("last").getString

Parser.parseString(getFirstName, jsString)  // Success("Pika")
Parser.parseString(getLastName, jsString)  // Success("chu")
```

Once we know the exact data we need, we can only parse things we need and ignore the rest (to a certain degree.)

For example, given a json

`[2, 4, 6, 8, ........]`,

if we only need the 2nd element, we don't care the type of 3rd element and so on, we don't even have to make sure the json is valid, as long as we can get the 2nd element.

All parsing logic lives in `basil.parser.JsonArrayParse`, I tried to be generic here, so the core logic can work with any input type as long as they can implement the typeclass required

### Acknowledgement

This project get a lot of idea from https://github.com/nuttycom/xenomorph, it shows how to retain an extra type parameter with structure that is similar to `Fix`.

I recommend this [video](https://www.youtube.com/watch?v=oRLkb6mqvVM) if you are interested

This library also make use of [magnolia](https://github.com/propensive/magnolia) project to derive ParseOps typeclass for case classes

### Release

Release is done with [sbt-sonatype](https://github.com/xerial/sbt-sonatype)

```
sbt publishSigned
sbt sonatypeRelease
```

### Next

* Benchmark
* Better error message
* Support extract json number into different scala number type (long, float, int, double)
* Support extract data as Sequence
* Support auto decoding of case classes
* Document how HFix and HFunctor work
