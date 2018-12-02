## Basil

![Build](https://travis-ci.org/buaya91/basil.svg?branch=master)

A json `Decoder` that can extract data from partial, incomplete json.

The main idea is to describe the data you need from json as recursive data structure called `ParseOps`, which then get interpreted into a `Parse` function which have a signature of `String => YourData` that you can use to parse json, the actual function signature is slightly more complex, but the idea is the same.

Json from `Array[Char]` is not supported now because Array cannot form a Monad trivially, one could convert `Array[Char]` to `List[Char]` for now.

**Warning**: This library is not production ready yet as I haven't done any benchmarking.

### Features

* Extract data from partial json, eg. `fs2.Stream[Char]`
* No intermediate json ast, eg. you get `"mystring"` instead of `JString("mystring")`
* Composable parse tree via FreeApplicative, eg. `GetString + GetNum => GetStringAndNum`
* Extract data for case class automatically via `basil-derive` module


### Example

```scala
import basil.data.ParseOpsConstructor._
import basil.parser.implicits._
import basil.parser._

val completeJS = s"""{"key1": "valueA", "key2": 2020.111}"""

val inCompleteJS = s"""{"key1": "valueA", "key2":2020.111, "key3":[200,]}"""


val parseOps = Start.getKey("key2").getNum.t

val result = Parser.parseJS(parseOps, completeJS.toCharArray.toList).head.map(_._1)

result == 2020.111

```

For more example, check out the test:

[Json Parsing example](./core/src/test/scala/basil/parser/ParseSpec.scala)
[Case class parsing example](./derive/src/test/scala/basil/derive/DeriveParseSpec.scala)

### How to compose ParseOps?

todo!!

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

All parsing logic lives in `basil.parser.JsonParse`, I tried to be generic here, so the core logic can work with any input type as long as they can implement the typeclass required

### Acknowledgement

This project get a lot of idea from https://github.com/nuttycom/xenomorph, it shows how to retain an extra type parameter with structure that is similar to `Fix`.

I recommend this [video](https://www.youtube.com/watch?v=oRLkb6mqvVM) if you are interested

This library also make use of [magnolia](https://github.com/propensive/magnolia) project to derive ParseOps typeclass for case classes

### Next

* Benchmark
* Support Option[X]
* Split fs2 into separate module
* Better error message
* Support extract json number into different scala number type (long, float, int, double)
* Support extract data as Sequence
* Support auto decoding of case classes
* Document how HFix and HFunctor work
