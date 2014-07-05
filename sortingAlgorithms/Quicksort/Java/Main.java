import java.util.Random;

/**
 * Testing module for QuickSort.
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
        Integer[] test = new Integer[20];
        
        System.out.println( "*** Creating new array ..." );
        System.out.println( "*** Filling in 20 random values ..." );

        for( int i = 0; i < test.length; i++ )
            test[i] = rnd.nextInt( 100 );

        System.out.println( "\n  ### Current array: \n" );
        System.out.print( "\t" );
        for( Integer elem : test )
            System.out.print( elem + " " );
        System.out.println( "\n\n" );

        System.out.println( "*** Sorting it (in-situ) ... ");

        QuickSort.quickSort( test );

        System.out.println( "\n ### Sorted array: \n");
        System.out.print( "\t" );
        for( Integer elem : test )
            System.out.print( elem + " " );
        System.out.println();
    }
}
