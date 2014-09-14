/**
 * Testing module for a Graph.
 *
 * @version 2014/09/14
 * @author  Thomas Lang
 */
public class Main {
    
    /**
     * Main method.
     * This method does some basic 
     * operations on a Graph.
     */
    public static void main( String[] args ){
	    Graph<Integer> test = new Graph<Integer>();
	    System.out.println( "*** Initializing new graph ... done." );

        test.add( 1, 2, 7   );
        test.add( 2, 3, 42  );
        test.add( 1, 3, 999 );
        test.add( 3, 4, 212 );

        System.out.println( "*** Adding sample edges ... done." );
        System.out.println( "\n   ### Current graph: " );
        System.out.println( test );

	    System.out.println( "   ### Size (number of edges): " + test.size() + "\n" );

        test.remove( 4 );
        System.out.println( "*** Removing node #\"4\" ... done." );
        System.out.println( "\n   ### Current graph: " );
        System.out.println( test );

        System.out.println( "   ### Size (number of edges): " + test.size() );
    }
}
