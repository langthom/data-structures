/**
 * Implementation of the Bubble sort algorithm.
 *
 * @author Thomas Lang
 * @version 0.1
 */
public class BubbleSort{

    /**
     * Implementation of the Bubble sort algorithm.
     *
     * @param  T[] array
     *                   The array to sort
     */
    public static <T extends Comparable<T>> void bubbleSort( T[] array ){
        for( int i = array.length; i > 0; i-- ){
            for( int j = 0; j < i - 1; j++ ){
                if( array[j].compareTo( array[j+1] ) > 0 ){
                    T tmp = array[j];
                    array[j] = array[j+1];
                    array[j+1] = tmp;
                }
            }
        }
    }
}
