import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;

/**
 * Selection sort-class.
 *
 * @author Thomas Lang
 * @version 0.1
 */
public class SelectionSort<T extends Comparable<T>>{

    private List<T> list;

    /**
     * Constructor, takes a variable number of
     * arguments and fills them in a LinkedList.
     *
     * @param  elements  The elements to fill in
     */
    public SelectionSort(T ... elements){
        list = new LinkedList();
        for( T elem : elements )
            list.add( elem );
    }

    /**
     * Sorting function, performs the selection
     * sort algorithm.
     *
     * @return  A sorted list.
     */
    public List<T> sort(){
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
    private T findMinimum( List<T> list ){
        ListIterator<T> it = list.listIterator();
        T min = list.get( 0 );
        while( it.hasNext() ){
            if( it.next().compareTo( min ) < 0 )
                min = it.previous();
        }
        return min;
    }  

    /**
     * String-representation.
     *
     * @return The String representation of the list.
     */
    public String toString(){
        return list.toString();
    }
}
