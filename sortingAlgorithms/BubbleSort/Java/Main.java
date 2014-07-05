import java.util.Random;

/**
 * Testing Module for the bubble sort algorithm. 
 *
 * @author Thomas Lang
 * @version 0.2
 */
public class Main{

    /**
     * Main-method, does the testing.
     */
    public static void main( String[] args ){
        Random rnd = new Random();
        Integer[] array = new Integer[20]; 

        for( int i = 0; i < array.length; i++ )
            array[i] = rnd.nextInt( 100 );

        System.out.println( "### Actual list: " );
        for( Integer elem : array )
            System.out.print( elem + " " );
        System.out.println();
        System.out.println( "### Sorting list ..." );

        BubbleSort.bubbleSort( array );

        System.out.println( "### Sorted list: ");
        for( Integer elem : array )
            System.out.print( elem + " " );
        System.out.println();
    }
}
