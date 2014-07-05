import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;

/**
 * Selection sort-class.
 *
 * @author Thomas Lang
 * @version 0.2
 */
public class SelectionSort{

    /**
     * Sorting function, performs the selection
     * sort algorithm.
     *
     * @return  A sorted list.
     */
    public static <T extends Comparable<T>> List<T> selectionSort( List<T> list ){
        List<T> res = new LinkedList<T>();
        T minimum = null;

        while( !list.isEmpty() ){
            minimum = findMinimum( list );
            res.add( minimum );
            list.remove( minimum );
        }
        return res;
    }

    /**
     * Helper function for finding the minimum 
     * of the actual list.
     *
     * @param  list  The list to search in.
     * @return The minimum of the list
     */
    private static <T extends Comparable<T>> T findMinimum( List<T> list ){
        ListIterator<T> it = list.listIterator();
        T min = list.get( 0 );
        while( it.hasNext() ){
            if( it.next().compareTo( min ) < 0 )
                min = it.previous();
        }
        return min;
    }  
}
