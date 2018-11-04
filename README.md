## Basil

![Build](https://travis-ci.org/buaya91/basil.svg?branch=master)

A json `Decoder` that can extract data from partial, incomplete json.

Right now it supports parsing json from `fs2.Stream[IO, Char]` and `List[Char]`

Support of `Array[Char]` (which is likely the most common usecase) is planned, but not implemented yet.

**Warning**: This library is likely buggy and not completely comply to JSON specification as of now

### Example

```scala
import basil.data.ParseOpsConstructor._
import basil.parser.implicits._
import basil.parser._

val completeJS = s"""{"key1": "valueA", "key2": 2020.111}"""

val inCompleteJS = s"""{"key1": "valueA", "key2":2020.111, "key3":[200,]}"""


val parseOps = Start.getKey("key2").getNum.t

val result = Parser.parseJS(parseOps, completeJS.toCharArray.toList).head.map(_._1)

result == JDouble(2020.111)

```

### How it works

The main idea is to define the data you want from the json as as a ParseOps, which is a GADT that can be recursive by using `Fix`.

Once we know the exact data we need, we can only parse things we need and ignore the rest (to a certain degree.)

For example, given a json

`[2, 4, 6, 8, ........]`,

if we only need the 2nd element, we don't care the type of 3rd element and so on, we don't even have to make sure the json is valid, as long as we can get the 2nd element.

All parsing logic lives in `basil.parser.JsonParse`, I tried to be generic here, so the core logic can work on any Json ADT and any raw data type as long as it implements the typeclassed needed.


### Next

* Handle escape char
* Handle whitespace
* Solve problem of type widening with Fix (maybe Free will help?)
* Support Array[Char]
* Support auto decoding of case classes
* Stack safety
* Benchmark
* Consider support serialization?


