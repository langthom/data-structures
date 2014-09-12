/**
 * Testing module for an AVL tree.
 *
 * @author Thomas Lang
 * @version 2014/09/02
 */
public class Main{

    /**
     * Main testing method.
     */
    public static void main( String[] args ){
        System.out.println( "*** Initializing new AVL tree ... done." );
        AVLTree<Integer> test = new AVLTree<Integer>();

        System.out.println( "*** Adding values 7, 42, 999, 0, 212, 512, 21 ... done." );

        test.insert( 7   );
        test.insert( 42  );
        test.insert( 999 );
        test.insert( 1   );
        test.insert( 212 );
        test.insert( 512 );
        test.insert( 21  );

        System.out.println( "\n   ### Current AVL tree: " );
        test.print();
        System.out.println( "\n   ### Number of Nodes: " + test.size() );
        System.out.println( "\n   ### Height of the tree: " + test.height() );
    }
}
