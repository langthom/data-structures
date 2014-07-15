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
    public static void main( String[] args ) throws Exception {
        System.out.println( "*** Initializing new MinHeap ... done." );
        System.out.println( "*** Adding values 7, 42, 999, 0 ... done." );

        MinHeap<Integer> test = new MinHeap<Integer>();

        test.insert( 7   );
        test.insert( 42  );
        test.insert( 999 );
        test.insert( 0   );
        test.insert( 212 );
        test.insert( 512 );
        test.insert( 21  );

        System.out.println( "   ### Current MinHeap: " );
        test.print();
        System.out.println( "\n   ### Number of nodes: " + test.getSize() );

        System.out.println( "\n*** Performing \"deleteMin\" ... done." );
        test.deleteMin();

        System.out.println( "   ### Current MinHeap: " );
        test.print();

        System.out.println( "\n   ### Number of nodes: " + test.getSize() );
    }
}
