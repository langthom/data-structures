/**
 * Testing Module for the bubble sort algorithm. 
 *
 * @author Thomas Lang
 * @version 0.1
 */
public class UsesBubbleSort{

    /**
     * Main-method, does the testing.
     */
    public static void main( String[] args ){
        BubbleSort<Integer> test = new BubbleSort<Integer>();

        Integer[] array = {5,9,24,72,0,-5,18,999,212};

        System.out.println( "### Actual list: " );
        for( Integer elem : array )
            System.out.print( elem + " " );
        System.out.println();
        System.out.println( "### Sorting list ..." );

        test.bubbleSort( array );

        System.out.println( "### Sorted list: ");
        for( Integer elem : array )
            System.out.print( elem + " " );
        System.out.println();
    }
}
