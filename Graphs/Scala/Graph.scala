/* Copyright (c) 2016, Thomas Lang. All rights reserved.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */


/** Implementation of a simple graph.
  * 
  * A graph is a structure that has edges and nodes. Nodes are connected through
  * edges and new nodes can be added to the graph.
  *
  * @author Thomas Lang
  * @version 1.0, 2016-01-09
  */
abstract class Graph {
  /* Abstract type for edges. */
  type Edge

  /* Abstract type for nodes, that must at least provide connectability. */
  type Node <: NodeIntf

  /* Some abstract super type for nodes that provides connectability. */
  abstract class NodeIntf {
    def connectWith(node: Node): Edge
  }

  def nodes: List[Node]
  def edges: List[Edge]
  def addNode: Node
}

/** Refinement of the graph structure.
  * Here the additional information is known that the graph is directed.
  *
  * @author Thomas Lang
  * @version 1.0, 2016-01-09
  * @see Graph
  */
abstract class DirectedGraph extends Graph {

  /* Now edges are directed from an origin to a destination node. */
  type Edge <: EdgeImpl

  /* Simplest implementation of an edge type. */
  class EdgeImpl(origin: Node, dest: Node) {
    def from = origin
    def to = dest
  }

  /* Simple implementation of a connectible node. */
  class NodeImpl extends NodeIntf {
    /*
     * Now we know, how we can connect an instance of this class to another one:
     * As the edges only go into one direction, we make a path from this instance
     * to the passed one.
     *
     * NOTE: This passing of 'this' does interfere with the signature providid in
     *       the super class as we haven't specified what the type Node is exactly
     *       is ... The type system expects a Node, but we gave it a NodeImpl.
     *       
     *       HERE COMES THE MAGIC RIGHT OUT OF THE DRAGONS ASS:
     *       
     *       We say with this special syntax, that for this implementation, the
     *       'this' pointer of an instance of NodeImpl is something of type Node!
     */
    self: Node =>
    def connectWith(node: Node): Edge = {
      val edge = newEdge(this, node)
      edges = edge :: edges
      edge
    }
  }

  // Stupid factory functions only ...
  protected def newNode: Node
  protected def newEdge(from: Node, to: Node): Edge

  var nodes: List[Node] = Nil
  var edges: List[Edge] = Nil

  /* Adding a new node to the graph. */
  def addNode: Node = {
    val node = newNode
    nodes = node :: nodes
    node
  }
}

/** Most concrete implementation of our directed graph.
  * 
  * @author Thomas Lang
  * @version 1.0, 2016-01-09
  * @see DirectedGraph
  */
class ConcretedDirectedGraph extends DirectedGraph {
  type Edge = EdgeImpl
  type Node = NodeImpl

  /*
   * As we stated right above, that the type Node is a 'NodeImpl', we can now
   * instantiate 'NodeImpl', so the return type is okay.
   */
  protected def newNode: Node = new NodeImpl
  protected def newEdge(from: Node, to: Node): Edge = new EdgeImpl(from, to)
}

/** Testing object.
  *
  * @author Thomas Lang
  * @version 1.0, 2016-01-09
  */
object Main {
  def main(args: Array[String]) {
    val g: Graph = new ConcretedDirectedGraph
    val n1 = g.addNode
    val n2 = g.addNode
    val n3 = g.addNode

    n1.connectWith(n2)
    n2.connectWith(n3)
    n3.connectWith(n1)
  }
}

