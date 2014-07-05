import java.lang.reflect.Array;

/**
 * Implementation of the Merge Sort sorting algorithm.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class MergeSort{

    /**
     * Calling method.
     *
     * @param  T[] array
     *                   The array to sort.
     */
    public static <T extends Comparable<T>> void mergeSort( T[] array ){
        mergeSort( array, 0, array.length - 1 );
    }

    /**
     * Does the recursive stuff: splits the array, 
     * runs recursively over that halfs and then merges
     * the whole thing together.
     *
     * @param  T[] array
     *                   The array to sort.
     * @param  int lo
     *                Lower limit for sorting.
     * @param  int hi
     *                High limit for sorting.
     */
    private static <T extends Comparable<T>> void mergeSort( T[] array, int lo, int hi ){
        if( lo < hi ){
            int m = (hi + lo)/2;
            mergeSort( array, lo, m     );
            mergeSort( array, m + 1, hi );
            merge( array, lo, m, hi );
        }
    }

    /**
     * Merges the splitted arrays together.
     *
     * @param  T[] array
     *                   The array to sort.
     * @param  int lo
     *                  Lower index for sorting.
     * @param  int m
     *                  Middle index, where the split is.
     * @param  int hi
     *                  Upper index for sorting.
     */
    private static <T extends Comparable<T>> void merge( T[] array, int lo, int m, int hi ){
        int len = hi - lo + 1;

        // note: this is the only way of creating a generic array in Java
        T[] buffer = (T[]) Array.newInstance( array.getClass().getComponentType(), len );

        System.arraycopy( array, lo, buffer, 0, len );

        int i = 0; int j = m - lo + 1;
        int k = lo;

        while(( i <= m - lo ) && ( j <= hi - lo )){
            if(( buffer[i].compareTo( buffer[j] ) < 0 ) || ( buffer[i].compareTo( buffer[j] ) == 0 ))
                array[k++] = buffer[i++];
            else
                array[k++] = buffer[j++];
        }

        while( i <= m - lo )
            array[k++] = buffer[i++];
    }
}
