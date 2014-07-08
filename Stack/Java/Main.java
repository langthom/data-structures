import java.util.Random;

/**
 * Testing module for a Stack.
 *
 * @author Thomas Lang, 2014
 * @version 0.2
 */
public class Main{

    /**
     * Main method, does some testing.
     */
    public static void main( String[] args ){
        Random rnd = new Random();
        Stack<Integer> myStack = new Stack<Integer>();

        for( int i = 0; i < 20; i++ )
            myStack.push( rnd.nextInt( 100 ));

        System.out.println( "*** Creating empty Stack ... " );
        System.out.println( "*** Pushing 20 random values to Stack ... " );
        System.out.println( "  ### Actual stack: \n" );
        System.out.println( myStack );
        System.out.println( "*** Getting top element via \'top\': " + myStack.top() );
        System.out.println( "  ### Actual stack: \n" );
        System.out.println( myStack );
        System.out.println( "*** Receiving and deleting top element via \'pop\': " + myStack.pop() );
        System.out.println( "  ### Actual stack: \n" );
        System.out.println( myStack );
    }
}
