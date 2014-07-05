/**
 * Implementation of the QuickSort sorting algorithm (in-situ version).
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class QuickSort{

    /**
     * Called method, calls the extended version.
     *
     * @param  T[] array
     *                   The array to sort.
     */
    public static <T extends Comparable<T>> void quickSort( T[] array ){
        quickSort( array, 0, array.length-1 );
    }

    /**
     * Main implementation, runs recursively.
     *
     * @param  T[] array
     *                   The array to sort
     * @param  int lo
     *                  Position of pivot element pointer
     * @param  int hi
     *                  Upper limit of sorting
     */
    private static <T extends Comparable<T>> void quickSort( T[] array, int lo, int hi ){
        if( lo < hi ){
            T pivot = array[lo]; int left = lo + 1; int right = hi;
            do{
                // running over array with pointers until element found or collision
                while(( left <= hi ) && ( array[left].compareTo( pivot ) < 0 || array[left].compareTo( pivot ) == 0 )){ ++left;  }
                while(( right > lo ) && ( array[right].compareTo( pivot ) > 0 )){ --right; }

                if( left < right )
                    swap( array, left, right );

            }while( left < right );

            swap( array, lo, right );   // swapping pivot element to right position

            quickSort( array, lo, right-1 );
            quickSort( array, right+1, hi );
        }
    }

    /**
     * Swaps two elements in an array.
     *
     * @param  T[] array
     *                   The array to swap in.
     * @param  int lo   
     *                  First index.
     * @param  int hi   
     *                  Second index.
     */
    private static <T> void swap( T[] array, int lo, int hi ){
        T tmp = array[lo];
        array[lo] = array[hi];
        array[hi] = tmp;
    }
}
