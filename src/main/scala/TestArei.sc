// Step 1: Define the Binary Tree Structure
sealed trait Tree
case class Node(value: Int, left: Tree = Empty, right: Tree = Empty) extends Tree
case object Empty extends Tree

// Step 2: Reverse Function
def reverseTree(tree: Tree): Tree = tree match {
  case Empty => Empty
  case Node(value, left, right) =>
    Node(value, reverseTree(right), reverseTree(left))
}

// Helper function to print the tree (in-order traversal)
def printTree(tree: Tree): Unit = tree match {
  case Empty =>
  case Node(value, left, right) =>
    printTree(left)
    print(s"$value ")
    printTree(right)
}

// Example usage
object BinaryTreeExample extends App {
  val tree = Node(1,
    left = Node(2,
      left = Node(4),
      right = Node(5)
    ),
    right = Node(3,
      left = Node(6),
      right = Node(7)
    )
  )

  println("Original Tree (In-order Traversal):")
  printTree(tree)
  println()

  val reversedTree = reverseTree(tree)

  println("Reversed Tree (In-order Traversal):")
  printTree(reversedTree)
  println()
}

BinaryTreeExample.main(Array())
