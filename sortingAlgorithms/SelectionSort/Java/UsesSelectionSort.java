import java.util.List;
import java.util.LinkedList;

/**
 * Testing Module for the selection sort algorithm.
 *
 * @author Thomas Lang
 * @version 0.1
 */
public class UsesSelectionSort{

    /**
     * Main-method, does the testing.
     */
    public static void main( String[] args ){
        SelectionSort<Integer> test = new SelectionSort( 5, 9, 24, 72, 0, -5, 18, 999, 212 );

        System.out.println( "### Actual list: " );
        System.out.println( test );
        System.out.println( "### Sorting list ..." );

        List<Integer> sortedList = test.sort();

        System.out.println( "### Sorted list: ");
        System.out.println( sortedList );
    }
}
