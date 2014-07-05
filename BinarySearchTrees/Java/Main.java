// UsesBinaryTree.java - a testing class for binary search trees

/**
 * Testing class for the binary search tree.
 *
 * @author  Thomas Lang
 * @version 0.2
 */
public class Main{

    /**
     * Main method, does the testing.
     */
    public static void main( String[] args ){
        BinarySearchTree<Integer> tree = new BinarySearchTree<Integer>();

        System.out.println( "*** Initializing new tree of type Integer ..." );
        System.out.println( "*** Adding values: 5, 7, 9, 1, 4, 0, 6, 2, 42\n" );
        tree.add( 5 ); tree.add( 7 ); tree.add( 9 ); tree.add( 1 );
        tree.add( 4 ); tree.add( 0 ); tree.add( 6 ); tree.add( 2 ); 
        tree.add( 42 );

        System.out.println( "*** Printing actual tree: \n" );
        tree.printTree();

        System.out.println( "\n*** Traversing:\n### Preorder-travsersing: " );
        tree.traversePreorder();
        System.out.println();
        System.out.println( "### Inorder-traversing: " );
        tree.traverseInorder();
        System.out.println();
        System.out.println( "### Postorder-traversing: " );
        tree.traversePostorder();
        System.out.println();
        System.out.println( "### Breadth-first-traversion: " );
        tree.traverseBreadthFirst();
        System.out.println();

        System.out.println( "*** Deleting: 5\n" );
        tree.delete( 5 );

        System.out.println( "*** Printing actual tree: " );
        tree.printTree();
    }
}
