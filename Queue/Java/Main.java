import java.util.Random;

/**
 * Testing module for a Queue.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class Main{

    /**
     * Main method, does some testing.
     */
    public static void main( String[] args ){
        Random rnd = new Random();
        Queue<Integer> test = new Queue<Integer>();

        for( int i = 0; i < 20; i++ )
            test.enqueue( rnd.nextInt( 100 ));

        System.out.println( "*** Creating new Queue ..." );
        System.out.println( "*** Filling in 20 random values ..." );
        System.out.println( "\n  ### Current Queue: \n" );
        System.out.println( test );
        System.out.println( "\n*** Removing two elements ... " );
        test.dequeue(); test.dequeue();
        System.out.println( "\n  ### Current Queue: \n" );
        System.out.println( test );
        System.out.println( "*** Adding \"999\" ... " );
        test.enqueue( 999 );
        System.out.println( "\n  ### Current Queue: \n" );
        System.out.println( test );
    }
}
