// UsesBinaryTree.java - a testing class for binary search trees

public class Main{

    public static void main( String[] args ){
        BinarySearchTree<Integer> tree = new BinarySearchTree<Integer>();

        tree.add( 5 ); tree.add( 7 ); tree.add( 9 ); tree.add( 1 );
        tree.add( 6 ); tree.add( 2) ; tree.add( 42 );

        tree.printTree();

        tree.traversePreorder();
        tree.traverseInorder();
        tree.traversePostorder();
    }
}
