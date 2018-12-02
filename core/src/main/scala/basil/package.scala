import basil.data.{HFix, ParseOps}
import cats.free.FreeApplicative

package object basil {
  type ParseTree[I]          = HFix[ParseOps, I]
  type FreeParseOps[F[_], I] = FreeApplicative[ParseOps[F, ?], I]
}
