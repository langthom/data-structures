import java.util.List;

/**
 * Testing Module for the selection sort algorithm.
 *
 * @author Thomas Lang
 * @version 0.1
 */
public class UsesInsertionSort{

    /**
     * Main-method, does the testing.
     */
    public static void main( String[] args ){
        InsertionSort<Integer> test = new InsertionSort<Integer>();

        Integer[] arr = {5,9,24,72,0,-5,18,999,212};

        System.out.println( "### Actual list: " );
        for( Integer elem : arr )
            System.out.print( elem + " " );
        System.out.println();
        System.out.println( "### Sorting list ..." );

        List<Integer> res = test.insertionSort( arr );

        System.out.println( "### Sorted list: ");
        System.out.println( res );
    }
}
