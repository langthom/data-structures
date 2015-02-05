import java.util.List;

/**
 * Interface for communication with a {@code TreeMySet}.
 *
 * @author Thomas Lang
 * @version 1.0, 05/02/2015
 */
public interface MySet<T extends Comparable<T>> {

    /**
     * Gets the total number of elements stored in the set.
     *
     * @return The size of the set.
     */
    int size();
    
    /**
     * Creates a new {@code MySet} containing the union of the calling one and
     * the passed {@code other} set.<p>
     * The union of two sets describes all elements that are either contained
     * in the first or in the second set, or both.
     *
     * @param other
     *              The second set for the union which must not be {@code null}.
     * @return The union of the calling and the passed set.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              set is {@code null}.
     */
    MySet<T> union(MySet<T> other);

    /**
     * Creates a new {@code MySet} containing the intersection of the calling
     * one and the passed {@code other} set.<p>
     * The intersection of two sets describes all elements that are both
     * contained in the first and the second set.
     *
     * @param other
     *              The second set for the intersection which must not be 
     *              {@code null}.
     * @return The intersection of the calling and the passed set.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              set is {@code null}.
     */
    MySet<T> intersect(MySet<T> other);

    /**
     * Creates a new {@code MySet} containing the set difference between the
     * calling and the passed {@code other} set.
     * The set difference of two sets A and B describes all elements out of A
     * that are <b>not</b> contained in B.
     *
     * @param other
     *              The second set that will be deleted from the calling one.
     *              This set must not be {@code null}.
     * @return The set difference of the calling one and the passed set.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              set is {@code null}.
     */
    MySet<T> difference(MySet<T> other);
 
    /**
     * Gets a {@code List} containing all elements of the set.
     *
     * @return A list containing all elements of the set.
     */
    List<T> getAll();
    
    /**
     * Adds the passed {@code element} to the set.
     * 
     * @param element
     *              The new element to add.
     * @return  {@code true} if the element was added successfully, 
     *          {@code false} otherwise.
     */
    boolean add(T element);

    /**
     * Checks if the set contains the passe {@code element}.
     *
     * @param element
     *              The element to check if contained in the set.
     * @return  {@code true} if the element is contained in the set,
     *          {@code false} otherwise.
     */
    boolean contains(T element);

    /**
     * Checks if the set is empty.
     *
     * @return  {@code true} if the set is empty, {@code false} otherwise.
     */
    boolean isEmpty();

    /**
     * Checks if the set is a subset of the passed {@code parentSet}.
     *
     * @param parentSet
     *              The second set to proof that it is a parent set of the
     *              calling one, which must not be {@code null}.
     * @return  {@code true} if the calling set is a subset of the passed one,
     *          {@code false} otherwise.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              set is {@code null}.
     */
    boolean isSubsetOf(MySet<T> parentSet);

    /**
     * Checks if the passed set is a subset of the calling one.
     *
     * @param childSet
     *              The second set to proof that it is a subset of the calling
     *              one, which must not be {@code null}.
     * @return  {@code true} if the passed set is a subset of the calling one,
     *          {@code false} otherwise.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              set is {@code null}.
     */
    boolean hasSubset(MySet<T> childSet);

    /**
     * Removes the passed {@code element} from the set if contained.
     *
     * @param element
     *          The element to remove from the set.
     * @return  {@code true} if the element was removed successfully from the 
     *          set, {@code false} otherwise.
     */
    boolean remove(T element);

    /**
     * Adds the passed list of elements to the set.
     *
     * @param other
     *              The {@code List} of elements to add to the set, which must
     *              not be {@code null}.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              list is {@code null}.
     */
    void addAll(List<T> other);

    /**
     * Clears the entire set by removing all elements from it.
     */
    void clear();

    /**
     * Returns a String representation of the set by listing up all elements.
     *
     * @return A representation of the set.
     */
    String toString();
}
