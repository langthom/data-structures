import java.util.Random;
import java.util.List;
import java.util.LinkedList;

/**
 * Testing Module for the selection sort algorithm.
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
        List<Integer> test = new LinkedList<Integer>();

        for( int i = 0; i < 20; i++ )
            test.add( rnd.nextInt( 100 ));

        System.out.println( "### Actual list: " );
        System.out.println( test );
        System.out.println( "### Sorting list ..." );

        List<Integer> sortedList = SelectionSort.selectionSort( test );

        System.out.println( "### Sorted list: ");
        System.out.println( sortedList );
    }
}
