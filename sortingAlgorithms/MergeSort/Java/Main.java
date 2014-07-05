import java.util.Random;

/**
 * Testing module for the Merge sort algorithm.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class Main{

    /**
     * Main method, does the testing.
     */
    public static void main( String[] args ){
        Random rnd = new Random();
        Integer[] test = new Integer[20];

        for( int i = 0; i < test.length; i++ )
            test[i] = rnd.nextInt( 100 );

        System.out.println( "*** Creating new array ..." );
        System.out.println( "*** Filling array with random values ..." );
        System.out.println( "\n  ### Current array: \n" );
        System.out.print( "\t" );
        for( Integer elem : test )
            System.out.print( elem + " " );
        System.out.println( "\n*** Sorting it with mergeSort ... " );
        MergeSort.mergeSort( test );
        System.out.println( "\n  ### Sorted array: \n" );
        System.out.print( "\t" );
        for( Integer elem : test )
            System.out.print( elem + " " );
        System.out.println();
    }
}
