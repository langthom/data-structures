/* Copyright (c) 2015, Thomas Lang. All rights reserved.
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>
 */

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;
import java.util.stream.*;

/**
 * Implementation of a simple hash map.<p>
 * This is basically a list of map entries but the location of such an entry is
 * defined through a hash value of its key. The default capacity of an instance
 * of this class is {@code 16} entries, the default load factor is 
 * {@code 0.75}. This means that if the current size of the map multiplied with
 * the load factor is greater than the current capacity, the capacity is 
 * doubled.<p>
 * Per definition of a hash map, this data structure is <strong>NOT</strong>
 * thread-safe buts <strong>DOES</strong> permit one {@code null} key and any 
 * number of {@code } values.
 *
 * @param <K>
 *         Type for keys stored in this map what can be any sub type of
 *         {@code Object}.
 * @param <V>
 *         Type for values stored in this map what can be any sub type of 
 *         {@code Object}.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-08-10
 * @see java.util.Map
 * @see Node
 */
public class HashMap<K, V> implements Map<K, V> {

    /** The default initial capacity of the map. */
    private static final int INITIAL_CAPACITY = 16;

    /** The default load factor of the map. */
    private static final float DEFAULT_LOAD_FACTOR = 0.75f;

    /** The number of entries stored in the map. */
    private int size;

    /** The capacity of this map. */
    private int capacity;

    /** The factor that decides when to resize the map. */
    private float loadFactor;

    /** The internal list containing the entries. */
    private ArrayList<Map.Entry<K, V>> entries;

    /**
     * Initializes the map by creating it and setting the needed parameters.
     *
     * @param initialCapacity
     *          The initial capacity of the map, which (if not {@code null})
     *          must not be smaller than zero.
     * @param loadFactor
     *          The factor to decide at which fullness the map must be 
     *          resized which (if not {@code null}) must not be smaller than 
     *          zero.
     * @throws IllegalArgumentException
     *          An {@code IllegalArgumentException} will be thrown if either
     *          the passed {@code initialCapacity} or the passed
     *          {@code loadFactor} was not null and is smaller than zero.
     */
    private void init(Integer initialCapacity, Float loadFactor) {

        if ((initialCapacity != null) && (initialCapacity < 0)) {
            throw new IllegalArgumentException(
                    "Illegal initial capacity: " + initialCapacity);
        }

        if ((loadFactor != null) && (loadFactor < 0)) {
            throw new IllegalArgumentException(
                    "Illegal load factor: " + loadFactor);
        }

        size = 0;
        int capacity = INITIAL_CAPACITY;
        float lFactor = DEFAULT_LOAD_FACTOR;

        if (initialCapacity != null) {
            capacity = initialCapacity;
        }

        if (loadFactor != null) {
            lFactor = loadFactor;
        }

        entries = new ArrayList<Map.Entry<K, V>>(capacity);
        this.capacity = capacity;
        this.loadFactor = lFactor;

        for (int i = 0; i < capacity; ++i) {
            entries.add(null);
        }
    }

    /**
     * Creates a new {@code HashMap} with the default initial capacity of 
     * {@code 16} and the default load factor of {@code 0.75}.
     *
     * @see #init(int, float)
     */
    public HashMap() {
        init(null, null);
    }

    /**
     * Creates a new {@code HashMap} with the passed {@code initialCapacity}
     * and the default load factor of {@code 0.75}.
     *
     * @param initialCapacity
     *          The initial capacity of the map.
     * @see #init(int, float)
     */
    public HashMap(int initialCapacity) {
        init(initialCapacity, null);
    }

    /**
     * Creates a new {@code HashMap} with the passed {@code initialCapacity}
     * and the passed {@code loadFactor}.
     *
     * @param initialCapacity
     *          The initial capacity of the map.
     * @param loadFactor
     *          The factor essential for deciding when to resize the map.
     * @see #init(int, float)
     */
    public HashMap(int initialCapacity, float loadFactor) {
        init(initialCapacity, loadFactor);
    }

    /**
     * Copy constructor for a whole {@code HashMap}.
     *
     * @param map
     *           The map to copy which must not be {@code null}.
     * @throws NullPointerException
     *           A {@code NullPointerException} will be thrown if the passed
     *           {@code map} was {@code null}.
     */
    public HashMap(Map<? extends K, ? extends V> map) {

        if (map == null) {
            throw new NullPointerException("Passed map was null");
        }

        init(map.size(), null);
        putAll(map);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        init(null, null);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void putAll(Map<? extends K, ? extends V> map) {

        if (map == null) {
            throw new NullPointerException("Map was null.");
        }

        map.entrySet().stream()
           .forEach(e -> put(e.getKey(), e.getValue()));
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsKey(Object key) {
        final Set<K> keySet = keySet();
        return keySet == null ? false : keySet.contains(key);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean containsValue(Object value) {
        final Collection<V> values = values();
        return values == null ? false : values.contains(value);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean equals(Object other) {
        if (other == this) {
            return true;
        } else if (other == null) {
            return false;
        } else if (!(other instanceof HashMap)) {
            return false;
        } else {
            HashMap<K, V> map = (HashMap<K, V>) other;
            return entrySet().equals(map.entrySet());
        }
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean isEmpty() {
        return size == 0;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int hashCode() {
        return entries.stream().mapToInt(entry -> entry.hashCode()).sum();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public int size() {
        return size;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<K> keySet() {
        Set<K> keys = new HashSet<>();
        entries.stream().forEach(e -> {
            if (e != null) {
                keys.add(e.getKey());
            }
        });
        return keys;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<Map.Entry<K, V>> entrySet() {
        return new HashSet<Map.Entry<K, V>>(entries);
    }

    /**
     * Computes the index belonging to the passed {@code key}.
     * This is basically the computed hash value broken down to the capacity.
     *
     * @param key
     *         The key to compute its index.
     * @return Returns the index for the passed {@code key}.
     * @see #hash(K)
     */
    private int indexOf(K key) {
        assert key != null : "Null key passed.";
        return hash(key) & (capacity - 1);
    }

    /**
     * Computes and returns the hash value belonging to the passed {@code key}.
     *
     * @param key
     *         The key to hash.
     * @return Returns the hash value for the passed {@code key}.
     */
    private int hash(K key) {
        assert (key != null) : "Null key passed.";

        int hash = key.hashCode();
        hash ^= (hash >>> 20) ^ (hash >>> 12);
        return hash ^ (hash >>> 7) ^ (hash >>> 4);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public V get(Object key) {

        if (!containsKey(key)) {
            return null;
        }

        int index = indexOf((K) key);
        Map.Entry<K, V> entry = entries.get(index);
        V value = entry.getValue();
        int visited = 0;

        /* Linear chaining. */
        while ((value == null) && (visited <= size)) {
            entry = entries.get((index + 1) % size);
            value = entry == null ? null : entry.getValue();
            ++visited;
        }

        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public V put(K key, V value) {
        int index = indexOf(key);
        int visited = 0;
        final int buckets = entries.size();

        if (key == null) {
            /*
             * As only one null value is permitted as key, this must be always
             * the very first entry.
             */
            V _value = get(key);
            entries.set(index, new Entry(key, value));
            return _value;
        }

        ListIterator<Map.Entry<K, V>> it = entries.listIterator(index);

        /* Linear chaining. */
        while (it.hasNext() && (visited <= buckets)) {
            Map.Entry<K, V> entry = it.next();
            V val = entry == null ? null : entry.getValue();

            if (val == null) {
                it.set(new Entry(key, value));
                break;
            } else if (it.nextIndex() >= buckets) {
                it = entries.listIterator();
            }

            ++visited;
        }

        ++size;

        if (loadFactor * buckets <= size) {
            Map<K, V> m = new HashMap<K, V>(this);

            /* Double the capacity of the new map if resizing necessary. */
            entries.ensureCapacity(2 * size);
            capacity *= 2;

            /* Rehashing the entire map. */
            m.entrySet().stream().forEach(e -> put(e.getKey(), e.getValue()));
        }

        return null;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public V remove(Object key) {

        final int index = indexOf((K) key);
        final int buckets = entries.size();
        int visited = 0;
        V value = null;
        ListIterator<Map.Entry<K, V>> it = entries.listIterator(index);

        /* Linear chaining. */
        while (it.hasNext() && (visited <= buckets)) {
            Map.Entry<K, V> curEntry = it.next();
            V curValue = curEntry == null ? null : curEntry.getValue();

            if (curValue != null) {
                value = curValue;
                it.set(null);
                break;
            } else if (it.nextIndex() >= buckets) {
                it = entries.listIterator();
            }
        }

        --size;
        return value;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Collection<V> values() {
        return entries.stream()
                      .map(e -> e.getValue()).collect(Collectors.toList());
    }

    /**
     * Returns a string representation of the map.
     *
     * @return Returns a string representation of the map.
     */
    @Override
    public String toString() {
        if (entries == null) {
            return "(empty map)";
        } else {
            StringBuilder repr = new StringBuilder();
            repr.append("|");
            entries.stream().forEach(e -> {
                if (e != null) {
                    repr.append(e);
                    repr.append("|");
                } else {
                    repr.append("-|");
                }
            });
            return repr.toString();
        }
    }

    /**
     * Implementation of a single map entry.<p>
     * Such an entry does save two things, namely its key and value. As usual
     * in a hash map, neither {@code null} keys nor {@code null} values are
     * permitted.
     *
     * @param <K>
     *         The type for keys of this entry, which can be any sub type of
     *         {@code Object}.
     * @param <V>
     *         The type for values of this entry, which can be any sub type of
     *         {@code Object}.
     *
     * @author Thomas Lang
     * @version 1.0, 2015-08-10
     * @see java.util.Map.Entry
     */
    static class Entry<K, V> implements Map.Entry<K, V> {

        /** Key of this map entry. */
        private K key;

        /** Value of this map entry. */
        private V value;

        /**
         * Creates a new map entry with {@code value} assigned to the passed 
         * {@code key}.
         *
         * @param key
         *          The key to store in this entry, which must not be 
         *          {@code null}.
         * @param value
         *          The value to store in this entry, which must not be 
         *          {@code null}.
         * @throws NullPointerException
         *          A {@code NullPointerException} will be thrown if either the
         *          passed {@code key} or the passed {@code value} was 
         *          {@code null}.
         */
        public Entry(K key, V value) {

            if (key == null) {
                throw new NullPointerException("Null key not permitted.");
            }

            if (value == null) {
                throw new NullPointerException("Null value not permitted.");
            }

            this.key   = key;
            this.value = value;
        }

        /**
         * Copy constructor that creates a new entry by deep copying the 
         * existing one.
         *
         * @param entry
         *          The map entry to copy which must not be {@code null}.
         * @throws NullPointerException
         *          A {@code NullPointerException} will be thrown if the passed
         *          entry was {@code null}.
         */
        public Entry(Entry<? extends K, ? extends V> entry) {
            
            if (entry == null) {
                throw new NullPointerException("Cannot create null entry.");
            }

            key   = entry.key;
            value = entry.value;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public boolean equals(Object other) {
            if (other == this) {
                return true;
            } else if (other == null) {
                return false;
            } else if (!(other instanceof Entry)) {
                return false;
            } else {
                synchronized (this) {
                Entry<K, V> e = (Entry<K, V>) other;
                boolean keyEq 
                    = key == null ? e.key == null : key.equals(e.key);
                boolean valEq 
                    = value == null ? e.value == null : value.equals(e.value);
                return keyEq && valEq;
                }
            }
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public K getKey() {
            return key;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public V getValue() {
            return value;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public synchronized int hashCode() {
            int keyHash = key == null ? 0 : key.hashCode();
            int valHash = value == null ? 0 : value.hashCode();
            return keyHash ^ valHash;
        }

        /**
         * {@inheritDoc}
         */
        @Override
        public synchronized V setValue(V value) {

            if (value == null) {
                throw new NullPointerException("Cannot set null value.");
            }

            V oldValue = this.value;
            this.value = value;
            return oldValue;
        }

        /**
         * Returns a string representation of a single entry.
         *
         * @return Returns a string representation of a single value.
         */
        @Override
        public String toString() {
            return "<" + key + ", " + value + ">";
        }
    }

    /**
     * Main (testing) function.
     *
     * @param args
     *          Command line arguments (not used here).
     */
    public static void main(String[] args) {
        System.out.print("Creating a new hash map ... ");
        HashMap<String, Integer> test = new HashMap<>();
        System.out.println("done.");

        System.out.print("Filling in three entries (no collision) ... ");
        test.put("First", 7);
        test.put("OMG", 999);
        test.put("Firs", 42);
        System.out.println("done.");
        System.out.println("Map now: ");
        System.out.println(test);

        System.out.println("Filling in entry (collision) ... ");
        test.put("First", 0);
        System.out.println("done.");
        System.out.println("Map now: ");
        System.out.println(test);

        System.out.print("Get value stored with key 'OMG': ");
        Integer val = test.get("OMG");
        System.out.println(val);

        System.out.print("Removing entry with key 'OMG': ");
        test.remove("OMG");
        System.out.println("done.");
        System.out.println("Does the tree contain an entry with the key 'OMG' "
                + test.containsKey("OMG"));

        System.out.println("Clearing map ... ");
        test.clear();
        System.out.println("done.");
    }
}
