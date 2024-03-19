import masks.JsonObject

import scala.annotation.tailrec

case class AVLNode(var key: Int,
                   var left: Option[AVLNode] = None,
                   var right: Option[AVLNode] = None,
                   var parent: Option[AVLNode] = None,
                   var balance: Int = 0,
                   value: JsonObject = JsonObject())


class AVLTree(var root: Option[AVLNode] = None) {
  private def treeRotateR(x: AVLNode): Unit = {
    val y: Option[AVLNode] = x.left
    x.left = y.get.right
    if (y.get.right.isDefined) {
      y.get.right.get.parent = Some(x)
    }
    y.get.parent = x.parent
    if (x.parent.isEmpty) {
      root = y
    } else if (x.parent.get.right.contains(x)) {
      x.parent.get.right = y
    } else {
      x.parent.get.left = y
    }
    y.get.right = Some(x)
    x.parent = y
  }

  private def treeRotateL(x: AVLNode): Unit = {
    val y: Option[AVLNode] = x.right
    x.right = y.get.left
    if (y.get.left.isDefined) {
      y.get.left.get.parent = Some(x)
    }
    y.get.parent = x.parent
    if (x.parent.isEmpty) {
      root = y
    } else if (x.parent.get.left.contains(x)) {
      x.parent.get.left = y
    } else {
      x.parent.get.right = y
    }
    y.get.left = Some(x)
    x.parent = y
  }

  private def treeRotateLR(x: AVLNode): Unit = {
    treeRotateL(x.left.get)
    treeRotateR(x)
  }

  private def treeRotateRL(x: AVLNode): Unit = {
    treeRotateR(x.right.get)
    treeRotateL(x)
  }

  private def avlRestoreBalance(x: AVLNode): Unit = {
    x.balance = treeHeight(x.right) - treeHeight(x.left)

    if (x.balance < -1) {
      if (treeHeight(x.left.get.left) > treeHeight(x.left.get.right)) {
        treeRotateR(x)
      } else {
        treeRotateLR(x)
      }
    }
    if (x.balance > 1) {
      if (treeHeight(x.right.get.right) > treeHeight(x.right.get.left)) {
        treeRotateL(x)
      } else {
        treeRotateRL(x)
      }
    }
  }

  def insert(key: Int, value: JsonObject = JsonObject()): Unit = {
    AVLInsert(AVLNode(key, value = value))
  }

  private def AVLInsert(x: AVLNode): Unit = {
    treeInsert(x)
    var current: Option[AVLNode] = Some(x)
    while (current.isDefined) {
      avlRestoreBalance(current.get)
      current = current.get.parent
    }
  }

  private def treeInsert(z: AVLNode): Unit = {
    val y = treeSearchInexact(z.key)
    z.parent = y
    if (y.isEmpty) {
      root = Some(z)
    } else if (z.key < y.get.key) {
      y.get.left = Some(z)
    } else if (z.key > y.get.key) {
      y.get.right = Some(z)
    }
  }

  def searchWithSteps(key: Int): (Option[AVLNode], Int) = {
    @tailrec
    def searchNode(node: Option[AVLNode], key: Int, steps: Int): (Option[AVLNode], Int) = {
      node match {
        case Some(n) =>
          if (key == n.key) (Some(n), steps)
          else if (key < n.key) searchNode(n.left, key, steps + 1)
          else searchNode(n.right, key, steps + 1)
        case None => (None, steps)
      }
    }

    searchNode(root, key, 0)
  }

  private def treeSearchInexact(k: Int): Option[AVLNode] = {
    var y: Option[AVLNode] = None
    var x = root
    while (x.isDefined && k != x.get.key) {
      y = x
      if (k < x.get.key) {
        x = x.get.left
      } else {
        x = x.get.right
      }
    }

    if (x.isDefined) {
      y = x
    }

    y
  }

  private def treeHeight(node: Option[AVLNode]): Int = {
    node match {
      case Some(n) => 1 + Math.max(treeHeight(n.left), treeHeight(n.right))
      case None => 0
    }
  }

  def displayTree(): Unit = {
    def displayNode(node: Option[AVLNode], level: Int): Unit = {
      if (node.isDefined) {
        displayNode(node.get.right, level + 1)
        println("  " * level + node.get.key)
        displayNode(node.get.left, level + 1)
      }
    }

    displayNode(root, 0)
  }

  def countNodes(): Int = {
    def countNodesRec(node: Option[AVLNode]): Int = {
      node match {
        case Some(n) => 1 + countNodesRec(n.left) + countNodesRec(n.right)
        case None => 0
      }
    }

    countNodesRec(root)
  }
}