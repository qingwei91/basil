## Ideas

### Operations as data

This project implemented the idea of model operations as data, which is a common technique in functional programming.

The main feature of this project is to allow extracting specific data from Json, for example given

```json
{
    "name": "Goku",
    "age": 200,
    "gender": "Male"
}
```

the data consumer want to know about the name, then he/she should be able to say something like `_.getKey("name").asString`, and get `"Goku"`, such operation is already supported in most if not all json library (with different syntax), but it is typically done in following steps:

1. Parse the whole json string into a Json AST
2. Extract data from Json AST

In many cases, **we dont need an intermediate Json AST**, if all we care is the `name` field, why do we need to parse `age` and `gender` field ?

This is what `basil` is trying to solve, by only extracting data without having to read the whole json, it allows us to work in streaming context, we can read json data from a partial json without having to load the whole data into memory.

To achieve this, instead of representing json as a AST, we need to be able to represent the action of `extracting data from raw Json`


#### Operations as functions
```scala
def getString[F[_]]: RawJson => F[JString] = ???
def getNum[F[_]]: RawJson => F[JNumber]
def getBool[F[_]]: RawJson => F[JBoolean]
def getKey[F[_], Json](key: String, next: RawJson => F[Json]): RawJson => F[Json]
```

This solution is straightforward, notice the `getKey` method takes a `next` parameter which can be recursive to allow extracting data with arbitrary layers of nesting.

This solution is pretty neat at first glance, every operation is a function and thus we can compose them in referential transparent way.

To implement them we need to know what `RawJson` and `F[_]` is, let's try to define them (note: it is possible to enforce constraint using typeclasses, but I don't think we can implement the methods with unbound type)

```scala
type RawJson = Array[Char]
type F[A] = Try[A]
```

Now we can implement these functions easily, but there are some issues,

* what if we want to parse something other than `Array[Char]`, eg. a stream?
* what if we want to use a different Json AST
* what if we want to produce a scalacheck generator instead of `Try[A]`

All of these questions point to a fundamental problem with this solution.

> The description of the problem is too tighly coupled with it's solution

What we really want is the ability to describe an action that extract data from Json without knowing the type of json, the type of input etc.

This project encodes the operations using a Generalized Algebraic Data Type (GADT)

#### GADT + recursion scheme
```scala
sealed trait ParseOps[+I]

case object GetString extends ParseOps[Nothing]

case object GetBool extends ParseOps[Nothing]

case object GetNum) extends ParseOps[Nothing]

case class GetKey[I](key: String, next: I) extends ParseOps[I]
```

Note: The real code used in `basil` is more complicated, for the sake of this document, I am using a simplified version.

By using GADT, there are 2 benefits:

1. Operations is now modelled as data that describe our problem domain, totally decoupled from the solution, ie. we can describe what data we want to extract without knowing the input type or the output type
2. Direct recursion is removed, recursion is achieved by using Fix point type.

To learn more about recursion schemes and fix point type, check README of [matryoshka](https://github.com/slamdata/matryoshka)

