package scalafix.rewrite
import scala.collection.immutable.Seq
import scala.meta.contrib._
import scala.meta.internal.ast.Helpers._
import scala.meta.{Symbol => _, _}
import scala.meta.semantic.v1._
import scalafix.syntax._
import scalafix.util.TreeExtractors._
import scalafix.util.Patch
import scalafix.util.TreePatch._
import scalafix.util.logger

case object Xor2Either extends Rewrite {
  override def rewrite(code: Tree, rewriteCtx: RewriteCtx): Seq[Patch] = {
    implicit val semantic = getMirror(rewriteCtx)
    code.collectFirst {
      case t: Term.Name
          if semantic
            .symbol(t)
            .toOption
            .exists(_.normalized == Symbol("_root_.cats.data.Xor.map.")) =>
        AddGlobalImport(importer"cats.implicits._")
    }.toList ++
      Seq(
        Replace(Symbol("_root_.cats.data.XorT."),
                q"EitherT",
                List(importer"cats.data.EitherT")),
        Replace(Symbol("_root_.cats.data.Xor."), q"Either"),
        Replace(Symbol("_root_.cats.data.Xor.Left."), q"Left"),
        Replace(Symbol("_root_.cats.data.Xor.Right."), q"Right"),
        Replace(Symbol("_root_.cats.data.XorFunctions.left."), q"Left"),
        Replace(Symbol("_root_.cats.data.XorFunctions.right."), q"Right"),
        RemoveGlobalImport(importer"cats.data.Xor"),
        RemoveGlobalImport(importer"cats.data.XorT")
      )
  }
}
