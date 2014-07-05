import java.util.Random;
import java.util.List;

/**
 * Testing Module for the selection sort algorithm.
 *
 * @author Thomas Lang
 * @version 0.1
 */
public class Main{

    /**
     * Main-method, does the testing.
     */
    public static void main( String[] args ){
        Random rnd = new Random();
        Integer[] arr = new Integer[20];

        for( int i = 0; i < arr.length; i++ )
            arr[i] = rnd.nextInt( 100 );

        System.out.println( "### Actual list: " );
        System.out.print( "[ " );
        for( Integer elem : arr )
            System.out.print( elem + ", " );
        System.out.print( "]" );
        System.out.println();
        System.out.println( "### Sorting list ..." );

        List<Integer> res = InsertionSort.insertionSort( arr );

        System.out.println( "### Sorted list: ");
        System.out.println( res );
    }
}
