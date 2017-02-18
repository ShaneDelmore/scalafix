package scalafix.rewrite

import scala.collection.immutable.Seq
import scala.meta.contrib._
import scala.meta.semantic.v1._
import scala.meta.{Symbol => _, _}
import scalafix.util.{Patch, TokenPatch}
import scalafix.util.TreePatch._
import RewriteHelpers._

case object Catz extends Rewrite {
  override def rewrite(code: Tree, rewriteCtx: RewriteCtx): Seq[Patch] = {
    implicit val semantic = getMirror(rewriteCtx)
    implicit val tree = code

    rewriteFunctor ++ rewriteSemigroup ++ rewriteEq
  }

  def rewriteFunctor(
      implicit code: Tree,
      rewriteCtx: ScalafixMirror
  ): Seq[Patch] =
    replaceGlobalImports(
      importer"cats.instances.option._" -> importer"scalaz.std.option._",
      importer"cats.instances.list._" -> importer"scalaz.std.list._",
      importer"cats.Functor" -> importer"scalaz.Functor"
    )

  def rewriteSemigroup(
      implicit code: Tree,
      rewriteCtx: ScalafixMirror
  ): Seq[Patch] =
    replaceGlobalImport(importer"cats.Semigroup" -> importer"scalaz.Semigroup") ++
      replaceTypeclassMember("Semigroup", "combine" -> "append")

  def rewriteEq(
      implicit code: Tree,
      rewriteCtx: ScalafixMirror
  ): Seq[Patch] =
    replaceGlobalImport(
      importer"cats.kernel.instances.int._" -> importer"scalaz.std.anyVal.intInstance") ++
      Seq(
        Replace(Symbol("_root_.cats.kernel.Eq.by."),
                q"Equal.equalBy",
                List(importer"scalaz.Equal")),
        Replace(Symbol("_root_.cats.kernel.Eq."),
                q"Equal",
                List(importer"scalaz.Equal")),
        RemoveGlobalImport(importer"cats.kernel.Eq")
      )

}

object RewriteHelpers {

  //If import is found replace it
  def replaceGlobalImport(importer: Importer, replacement: Importer)(
      implicit tree: Tree): Seq[Patch] =
    tree
      .collectFirst {
        case i: Importer if i.structure == importer.structure =>
          Seq(
            AddGlobalImport(replacement),
            RemoveGlobalImport(importer)
          )
      }
      .getOrElse(Nil)

  def replaceGlobalImport(imports: (Importer, Importer))(
      implicit tree: Tree): Seq[Patch] =
    replaceGlobalImport(imports._1, imports._2)

  def replaceGlobalImports(imports: (Importer, Importer)*)(
      implicit code: Tree,
      rewriteCtx: ScalafixMirror
  ): Seq[Patch] =
    imports.toIndexedSeq.flatMap(replaceGlobalImport)

  //I am infering this could be a typeclass by the matching name and implicit modifier.  Is there a better way
  //This only works for explicitly typed definitions
  //This feels low cost as we have an explicit to implicit rewrite
  def typeclassDefns(simpleTypeName: String)(
      implicit code: Tree,
      rewriteCtx: ScalafixMirror
  ): Seq[Tree] = {
    code.collect {
      case t @ Defn.Val(mods, _, Some(tpe), _)
          if mods.exists(_.syntax == "implicit") &&
            tpe.syntax.contains(simpleTypeName) =>
        t
      case t @ Defn.Def(mods, _, _, _, Some(tpe), _)
          if mods.exists(_.syntax == "implicit") &&
            tpe.syntax.contains(simpleTypeName) =>
        t
    }
  }

  def replaceTypeclassMember(
      typeclass: String,
      memberMapping: (String, String)
  )(
      implicit code: Tree,
      rewriteCtx: ScalafixMirror
  ): Seq[Patch] =
    typeclassDefns(typeclass)
      .flatMap(_.collect {
        case t @ Defn.Def(mods, name, _, _, _, _)
            if name.syntax == memberMapping._1 =>
          TokenPatch.AddLeft(name.tokens.head, memberMapping._2) +: name.tokens
            .map(TokenPatch.Remove)
      })
      .flatten

}
