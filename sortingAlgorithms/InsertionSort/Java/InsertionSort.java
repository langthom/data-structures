/**
 * Insertion sort-class.
 *
 * @author Thomas Lang
 * @version 0.2
 */
public class InsertionSort<T extends Comparable<T>>{

    T[] arr;

    /**
     * Constructor.
     * 
     * @param  elements  The array of elements to sort.
     */
    public InsertionSort( T ... elements ){
        arr = elements;
    }

    /**
     * Sorting method, performs the insertion
     * sort algorithm.
     *
     * @param  arr  The array to sort.
     */
    public void sort(){
        for( int i = 1; i < arr.length; i++ ){
            T value = arr[i];
            int j = i - 1;
            while( j >= 0 && arr[j].compareTo( value ) > 0 ){
                arr[j+1] = arr[j];
                j--;
            }
            arr[j+1] = value;
        }
    }

    /**
     * String-representation of the array.
     *
     * @return String-representation of the array.
     */
    public String toString(){
        StringBuilder out = new StringBuilder();
        out.append( "[ " );
        for( T elem : arr )
            out.append( elem + " " );
        out.append( "]" );
        return out.toString();
    }
}
