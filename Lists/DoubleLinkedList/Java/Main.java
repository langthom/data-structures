import java.util.Random;

/**
 * Testing module for the double linked list.
 *
 * @author Thomas Lang, 2014
 * @version 0.1
 */
public class Main{

    /**
     * Main method, does some testing.
     */
    public static void main( String[] args ){
        Random rnd = new Random();
        DoubleLinkedList<Integer> myList = new DoubleLinkedList<Integer>();

        for( int i = 0; i < 18; i++ )
            myList.add( rnd.nextInt( 100 ));
        myList.add( 7 );
        myList.add( 42 );

        System.out.println( "*** Creating empty Integer list ... " );
        System.out.println( "*** Inserting 20 random values ... " );
        System.out.println( "  ### Actual list: \n" );
        System.out.println( myList );
        System.out.println( "\n*** Inserting \"17\" after \"7\"... \n" );
        myList.insertAfter( 17, 7 );
        System.out.println( "  ### Actual list: \n" );
        System.out.println( myList );
        System.out.println( "*** Adding value \"999\" at the beginning ..." );
        myList.addFront( 999 );
        System.out.println( "  ### Actual list: \n" );
        System.out.println( myList );
        System.out.println( "*** Getting and deleting last element: " + myList.pop() );
        System.out.println( "\n  ### Actual list: \n" );
        System.out.println( myList );
    }
}
