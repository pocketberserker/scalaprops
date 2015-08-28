package scalaprops

import scalaz._
import scala.scalajs.js.annotation.JSExportDescendentObjects
import scala.scalajs.js.annotation.JSExportDescendentClasses

@JSExportDescendentObjects
@JSExportDescendentClasses
trait Scalaprops {

  def param: Param = Param.withCurrentTimeSeed()

  def listener: ScalapropsListener =
    ScalapropsListener.default

  def transformProperties[A](properties: List[Properties[A]]): List[Properties[A]] =
    properties.map(Scalaprops.filterUnitEmpty).sortBy(_.id.toString)

  /*-------------------------------------------------------------------------*\
  **  ScalaCheck                                                             **
  **  Copyright (c) 2007-2015 Rickard Nilsson. All rights reserved.          **
  **  http://www.scalacheck.org                                              **
  **                                                                         **
  **  This software is released under the terms of the Revised BSD License.  **
  **  There is NO WARRANTY. See the file LICENSE for the full text.          **
  \*------------------------------------------------------------------------ */
  private[scalaprops] val props = new scala.collection.mutable.ListBuffer[Properties[Any]]

  sealed class PropertySpecifier() {
    def update(propName: String, p: Property) = props += p.toProperties(propName)
  }

  lazy val property = new PropertySpecifier()

  sealed class PropertiesSpecifier() {
    def update[A](propName: String, p: Properties[A]) = {
      props += Properties.noSort[Any](
        Tree.node(
          propName -> Maybe.empty,
          p.mapId[Any](a => a).props #:: Stream.empty
        )
      )
    }
  }

  lazy val properties = new PropertiesSpecifier()
}

object Scalaprops {

  def filterUnitEmpty[A](p: Properties[A]): Properties[A] = {
    def loop(tree: Tree[(A, Maybe[Check])]): Tree[(A, Maybe[Check])] =
      tree match {
        case Tree.Node(root, Stream(Tree.Node((Or.L(()), Maybe.Empty()), sub))) =>
          Tree.node(root, sub.map(loop))
        case Tree.Node((root, Maybe.Empty()), Stream(Tree.Node(((), sub1), sub2))) =>
          Tree.node(root -> sub1, sub2.map(loop))
        case _ =>
          Tree.node(tree.rootLabel, tree.subForest.map(loop))
      }
    Properties.noSort(loop(p.props))
  }

}
