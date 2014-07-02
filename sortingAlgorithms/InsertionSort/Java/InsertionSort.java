import java.util.List;
import java.util.LinkedList;
import java.util.ListIterator;

/**
 * Insertion sort-class.
 *
 * @author Thomas Lang
 * @version 0.2
 */
public class InsertionSort<T extends Comparable<T>>{

    /**
     * Empty Constructor.
     */
    public InsertionSort(){}

    /**
     * Sorting method, performs the insertion
     * sort algorithm.
     */
    public static <T extends Comparable<T>> List<T> insertionSort( T[] array ){
        List<T> list = new LinkedList<T>();
        for( T element : array ){
            ListIterator<T> iterator = list.listIterator(); 
            boolean foundGreater = false;
            while( iterator.hasNext() && !foundGreater )
                foundGreater |= iterator.next().compareTo( element ) > 0;
            if( foundGreater )
                iterator.previous();
            iterator.add( element );
        }
        return list;
    }
}
