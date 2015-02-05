import java.util.Iterator;
import java.util.Collections;
import java.util.List;
import java.util.LinkedList;
import java.util.stream.*;

/**
 * Implementation of the interface {@code MySet}.<p>
 * This set implementation uses a {@code BinarySearchTree} as the internal data
 * container, which makes a lookup quite fast.
 * Generally, a Set is a mathematical object which allows no duplicates. A Set
 * can perform some specific operations, such as the {@code union} or the 
 * {@code intersection}.
 *
 * @author Thomas Lang
 * @version 1.0, 05/02/2015
 * @see MySet
 */
public class TreeMySet<T extends Comparable<T>> implements MySet<T> {

    /** The internal data container. */
    private BinarySearchTree<T> tree;

    /**
     * Creates a new {@code TreeMySet}.
     */
    public TreeMySet() {
        tree = new BinarySearchTree<T>();
    }

    /**
     * Creates a new {@code TreeMySet} and copies all elements from the passed
     * set into the new one.
     *
     * @param other
     *              The Set to copy.
     * @throws IllegalArgumentException
     *              An {@code IllegalArgumentException} is thrown if the passed
     *              {@code MySet} is {@code null}.
     */
    public TreeMySet(MySet<T> other) {
        this();

        if (other == null) {
            throw new IllegalArgumentException("Set to copy must not be null.");
        }

        addAll(other.getAll());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return tree.size();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MySet<T> union(MySet<T> other) {

        if (other == null) {
            throw new IllegalArgumentException("Second set must not be null.");
        }

        MySet<T> unionedMySet = new TreeMySet<T>();
        final List<T> unioned = getAll();
        unioned.addAll(other.getAll());
        Collections.sort(unioned);
        unionedMySet.addAll(unioned);
        return unionedMySet;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public List<T> getAll() {
        return tree.getAll();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public MySet<T> intersect(MySet<T> other) {

        if (other == null) {
            throw new IllegalArgumentException("Second set must not be null.");
        }

        MySet<T> intersectedSet = new TreeMySet<T>();
        final List<T> allOther = other.getAll();
        getAll().stream().forEach(elem -> 
                {
                    if (allOther.contains(elem)) {
                        intersectedSet.add(elem);
                    }
                });
        return intersectedSet;
    }

    /**
     * {@inheritDoc}
     */   
    @Override
    public MySet<T> difference(MySet<T> other) {

        if (other == null) {
            throw new IllegalArgumentException("Second set must not be null.");
        }

        MySet<T> differenceSet = new TreeMySet<T>();
        final List<T> allOther = other.getAll();
        getAll().stream().forEach(elem ->
                {
                    if (!allOther.contains(elem)) {
                        differenceSet.add(elem);
                    }
                });
        return differenceSet;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean add(T element) {
        return tree.add(element);
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    public void addAll(List<T> other) {

        if (other == null) {
            throw new IllegalArgumentException("List of elements must not be null.");
        }

        other.stream().forEach(element -> add(element));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean contains(T element) {
        return tree.contains(element);
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    public boolean isEmpty() {
        return tree.isEmpty();
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    public boolean remove(T element) {
        return tree.delete(element);
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    public boolean isSubsetOf(MySet<T> parentSet) {

        if (parentSet == null) {
            throw new IllegalArgumentException("Second set must not be null.");
        }

        final List<T> parents = parentSet.getAll();
        boolean isSubset = true;

        for (Iterator it = getAll().iterator(); it.hasNext(); ) {
            if (!parents.contains(it.next())) {
                isSubset = false;
                break;
            }
        }

        return isSubset;
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    public boolean hasSubset(MySet<T> childSet) {

        if (childSet == null) {
            throw new IllegalArgumentException("Second set must not be null.");
        }

        final List<T> all = getAll();
        boolean hasSubset = true;

        for (Iterator it = childSet.getAll().iterator(); it.hasNext(); ) {
            if (!all.contains(it.next())) {
                hasSubset = false;
                break;
            }
        }

        return hasSubset;
    }

    /**
     * {@inheritDoc}
     */ 
    @Override
    public void clear() {
        tree.clear();
    }
 
    /**
     * {@inheritDoc}
     */   
    @Override
    public String toString() {
        if (isEmpty()) {
            return "(empty set)";
        }
        return tree.toString();
    }

    /**
     * Testing function which performs some operations for testing purposes.
     */
    public static void main(String[] args) {
        MySet<Integer> setA = new TreeMySet<>();
        MySet<Integer> setB = new TreeMySet<>();

        for (int i = 1; i <= 4; ++i) {
            setA.add(i*i);
        }
   
        for (int i = 1; i <= 20; ++i) {
            setB.add(i);
        }

        System.out.println("*** Creating two new sets ... done.");
        System.out.println("*** Initializing with values ... done.");
        System.out.println("Set A: " + setA);
        System.out.println("Size of A: " + setA.size());
        System.out.println();
        System.out.println("Set B: " + setB);
        System.out.println("Size of B: " + setB.size());
        System.out.println();
        System.out.println("A intersects B: " + setA.intersect(setB));
        System.out.println("A unioned with B: " + setA.union(setB));
        System.out.println();
        System.out.println("A \\ B: " + setA.difference(setB));
        System.out.println("B \\ A: " + setB.difference(setA));
        System.out.println();
        System.out.println("A subset of B? -> " + setA.isSubsetOf(setB));
        System.out.println("B subset of A? -> " + setB.isSubsetOf(setA));
        System.out.println();
        
        MySet<Integer> oneFourSet = new TreeMySet<>();
        MySet<Integer> oneThreeSet = new TreeMySet<>();
        oneFourSet.add(1);
        oneFourSet.add(4);
        oneThreeSet.add(1);
        oneThreeSet.add(3);

        System.out.println("A has subset {1,4}? " + setA.hasSubset(oneFourSet));
        System.out.println("A has subset {1,3}? " + setA.hasSubset(oneThreeSet));
        System.out.println();
        System.out.println("*** Clearing set A ... done.");
        
        setA.clear();

        System.out.println("Set A now: " + setA);
    }
}
