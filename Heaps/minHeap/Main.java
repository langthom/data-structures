/**
 * Testing module for a Min-Heap.
 * 
 * @author Thomas Lang
 * @version July 15, 2014
 */
public class Main{

    /**
     * Main-method, does the testing.
     */
    public static void main( String[] args ){
        System.out.println( "*** Initializing new MinHeap ... done." );
        System.out.println( "*** Adding values 7, 42, 999, 0 ... done." );

        MinHeap<Integer> test = new MinHeap<Integer>();

        test.insert( 7   );
        test.insert( 42  );
        test.insert( 999 );
        test.insert( 0   );

        System.out.println( "   ### Current MinHeap: " );
        System.out.println( test );
    }
}
