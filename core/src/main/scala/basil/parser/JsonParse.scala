package basil.parser

import basil.data._
import basil.typeclass.Lazy
import cats.{Applicative, ~>}
import cats.arrow.FunctionK
import cats.data.NonEmptyMap
import cats.implicits._

trait JsonParse[Input, Output[_]] {
  type Parse[I] = Vector[PPath] => Input => Output[I]

  def parseString: Parse[String]
  def parseBoolean: Parse[Boolean]
  def parseNumber(terminator: ExpectedTerminator): Parse[Double]
  def parseArrayItem[I](n: Int, next: Parse[I]): Parse[I]
  def parseObj[I](k: String, nextOp: Parse[I]): Parse[I]
  def parseOneOf[I](oneOf: NonEmptyMap[String, Lazy[Parse[I]]]): Parse[I]
  def parseOptional[I](parse: Parse[I]): Parse[Option[I]]

  def parsing: ParseOps[Parse, ?] ~> Parse

  // default implementation can be provide if Parse forms a Functor
  // it should always be a Functor, but in case I am wrong
  // we dont enforce it on `parsing`
  def parsingM(implicit F: Applicative[Parse]): ParseOps[Parse, ?] ~> Parse =
    new (ParseOps[Parse, ?] ~> Parse) {
      override def apply[A](fa: ParseOps[Parse, A]): Parse[A] = {
        val parseA = fa match {
          case GetString                   => parseString
          case GetBool                     => parseBoolean
          case GetNum(t)                   => parseNumber(t)
          case getN: GetN[Parse, i]        => parseArrayItem(getN.n, getN.next)
          case getK: GetKey[Parse, i]      => parseObj(getK.key, getK.next)
          case GetSum(oneOf)               => parseOneOf(oneOf)
          case getOpt: GetOpt[Parse, i]    => parseOptional(getOpt.next)
          case mapped: Mapped[Parse, h, A] => mapped.fi.map(mapped.fn)
          case GetProduct(all)             => all.foldMap(FunctionK.id)
        }
        // is this harmful??
        parseA.asInstanceOf[Parse[A]]
      }
    }
}
