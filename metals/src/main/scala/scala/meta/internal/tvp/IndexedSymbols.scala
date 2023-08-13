package scala.meta.internal.tvp

import scala.collection.concurrent.TrieMap

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.Time
import scala.meta.internal.metals.Timer
import scala.meta.internal.semanticdb.SymbolInformation
import scala.meta.internal.semanticdb._
import scala.meta.io.AbsolutePath
import scala.meta.internal.mtags.GlobalSymbolIndex

// TODO make sure Java jars work
class IndexedSymbols(index: GlobalSymbolIndex, isStatisticsEnabled: Boolean) {
  private val cache = TrieMap.empty[
    AbsolutePath,
    TrieMap[String, Array[TreeViewSymbolInformation]],
  ]

  def clearCache(path: AbsolutePath): Unit = {
    cache.remove(path)
  }

  def reset(): Unit = {
    cache.clear()
  }

  def symbols(
      in: AbsolutePath,
      symbol: String,
  ): Iterator[TreeViewSymbolInformation] = {
    val symbols = cache.getOrElseUpdate(in, TrieMap.empty)
    symbols
      .getOrElseUpdate(
        symbol.asSymbol.enclosingPackage.value, {
          val timer = new Timer(Time.system)
          val result = loadSymbols(in, symbol)
          if (isStatisticsEnabled) {
            scribe.info(s"$timer - $in/!$symbol")
          }
          result
        },
      )
      .iterator
  }

  // TODO we might want to get toplevels first, which are already indexed and only on opening a node with file get methods etc
  private def loadSymbols(
      in: AbsolutePath,
      symbol: String,
  ): Array[TreeViewSymbolInformation] = {
    // TODO get proper dialect based on containing build target
    index
      .definitionsAt(in, scala.meta.dialects.Scala213)
      .map { symDef =>
        // TODO we might want to also store it while inexing
        val kind =
          if (symDef.definitionSymbol.isMethod) SymbolInformation.Kind.METHOD
          else if (symDef.definitionSymbol.isType) SymbolInformation.Kind.TYPE
          else if (symDef.definitionSymbol.isTypeParameter)
            SymbolInformation.Kind.TYPE_PARAMETER
          else if (symDef.definitionSymbol.isTerm) SymbolInformation.Kind.FIELD
          else SymbolInformation.Kind.CLASS
        TreeViewSymbolInformation(symDef.definitionSymbol.value, kind)
      }
      .toArray
  }
}
