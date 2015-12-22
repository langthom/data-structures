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
package mylist

import scala.annotation.tailrec

/**
 * Trait describing a simple single linked list.
 * Such a list can be either empty or be an element followed by a list.
 * 
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 * @param E
 *         Type parameter.
 */
trait MyList[+E] {
    
    /**
     * Checks if the list is empty or not.
     *
     * @return Returns either {{{true}}} if the list is empty or {{{false}}} 
     *         otherwise.
     */
    def isEmpty(): Boolean

    /**
     * Adds the passed value to the front of the list.
     * If the list is empty, a new list with this one entry will be created.
     * If the list is not empty, the element will be added at the front.
     *
     * @param value
     *         The value to insert into the list.
     * @return Returns the list with the element added.
     */
    def addFront[T >: E](value: T): MyList[T]

    /**
     * Adds the passed value to the rear end of the list.
     * If the list is empty, a new list with this one entry will be created.
     * If the list is not empty, the element will be added to the rear.
     *
     * @param value
     *         The value to insert into the list.
     * @return Returns the list with the element inserted.
     */
    def add[T >: E](value: T): MyList[T]

    /**
     * Operator for adding elements to the rear of the list, just a shortcut
     * for the real function.
     *
     * @param value
     *         The value to insert into the list.
     * @return Returns the list with the element inserted.
     * @see #add(T)
     */
    def +[T >: E](value: T): MyList[T] = add(value)

    /**
     * Inserts all elements from the passed {{{collection}}} into this list.
     *
     * @param collection
     *         A list of values to add to this list.
     * @return Returns the list with all the elements from the passed 
     *         {{{collection}}} added.
     */
    def ++[T >: E](collection: List[T]): MyList[T] = {
        @tailrec
        def recurse(acc: MyList[T], rest: List[T]): MyList[T] = rest match {
            case Nil => acc
            case _   => recurse(acc + rest.head, rest.tail)
        }

        recurse(this, collection)
    }

    /**
     * Concatenates two instances of this type.
     *
     * @param collection
     *         A list of values to add to this list.
     * @return Returns the list resulting from the concatenation.
     * @see #++(List[T])
     */
    def ++[T >: E](collection: MyList[T]): MyList[T] = ++(collection.toList)
    
    /**
     * Clears the list by removing all entries.
     *
     * @return Returns an empty list.
     */
    def clear[T >: E](): MyList[T]

    /**
     * Gets the element at the passed {{{index}}}.
     * If the index is out of the bounds of this list, {{{None}}} will be
     * returned, otherwise the element will be received.
     *
     * @param index
     *         The index of the wanted element in this list.
     * @return Returns the element at the passed index in this list, or
     *         {{{None}}} if the index is out of the bounds of this list.
     */
    def get[T >: E](index: Int): Option[T]

    /**
     * Removes the passed {{{value}}} from the list.
     * If this element is really contained in this list, it will be removed
     * and the list without this element will be returned. But if otherwise
     * the element is not in this list, the list will not be affected and
     * returned.
     *
     * @param value
     *         The value to remove from the list.
     * @return Returns this list with either the element removed if it was
     *         contained or the original list otherwise.
     */
    def remove[T >: E](value: T): MyList[T]

    /**
     * Removes the passed {{{value}}} from the list.
     * If this element is really contained in this list, it will be removed
     * and the list without this element will be returned. But if otherwise
     * the element is not in this list, the list will not be affected and
     * returned.
     *
     * @param value
     *         The value to remove from the list.
     * @return Returns this list with either the element removed if it was
     *         contained or the original list otherwise.
     * @see #remove(T)
     */
    def -[T >: E](value: T): MyList[T] = remove(value)
    
    /**
     * Returns the head of the list.
     * If the list is empty, it has no head and so {{{None}}} will be returned.
     * Otherwise, the first element is received.
     *
     * @return Returns the head of the list.
     */
    def head(): Option[E]

    /**
     * Returns the tail of the list.
     * If the list is empty, it has no tail, so we return the empty list.
     * Otherwise the tail list will be returned, that is this list without the
     * very first element.
     *
     * @return Returns the tail of this list.
     */
    def tail(): MyList[E]

    /**
     * Converts this list to a scala list.
     *
     * @return Returns a scala list with all the elements from this list.
     */
    def toList(): List[E]
}

/**
 * Representation of an empty list.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 */
case object MyNil extends MyList[Nothing] {
    /** @inheritdoc */
    def isEmpty = true
    
    /** @inheritdoc */
    def addFront[E](value: E) = MyList(value)
    
    /** @inheritdoc */
    def add[E](value: E) = MyList(value)

    /** @inheritdoc */
    def clear[E](): MyList[E] = MyNil

    /** @inheritdoc */
    def get[E](index: Int): Option[E] = None

    /** @inheritdoc */
    def remove[E](value: E): MyList[E] = MyNil

    /** @inheritdoc */
    def head = None

    /** @inheritdoc */
    def tail = MyNil

    /** @inheritdoc */
    def toList = Nil

    /**
     * Returns a string representation of the empty list.
     *
     * @return Returns a string representation of the empty list.
     */
    override def toString = "[]"
}

/**
 * Representation of an internal node.
 * Such a node has exactly one value and a reference to another list instance.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 * @param E
 *         Type parameter.
 */
case class Node[E](value: E, next: MyList[E]) extends MyList[E] {
    /** @inheritdoc */
    def isEmpty = false

    /** @inheritdoc */
    def addFront[T >: E](newvalue: T) = {
        def wrap(l: MyList[T]): MyList[T] = l
        wrap(Node(newvalue, wrap(this)))
    }

    /** @inheritdoc */
    def add[T >: E](newvalue: T) = withAdd(next + newvalue)

    /** @inheritdoc */
    def clear[T >: E]() = MyNil

    /** @inheritdoc */
    def get[T >: E](index: Int): Option[T] = {
        @tailrec
        def recurse(index: Int, tail: MyList[T]): Option[T] = (index, tail) match {
            case (0, t)            => t.head()
            case (i, t)            => recurse(i - 1, t.tail())
        }

        if (index < 0) None
        else if (index == 0) Some(value)
        else recurse(index, next)
    }

    /** @inheritdoc */
    def remove[T >: E](value: T): MyList[T] = {
        @tailrec
        def recurse(acc: MyList[T], ta: MyList[T]): MyList[T] = ta match {
            case MyNil                      => MyNil
            case Node(v, t) if (v == value) => acc ++ t
            case Node(v, t)                 => recurse(acc + v, t)
        }

        def wrap(n: MyList[E]): MyList[E] = n

        recurse(MyNil, wrap(this))
    }

    /**
     * Creates a node with this value and a reference to a new list.
     * This is only used for ensuring type safety.
     * 
     * @param newnext
     *         Reference to the new list.
     * @return Returns the newly created node.
     */
    def withAdd[T >: E](newnext: MyList[T]): MyList[T] = Node(value, newnext)
    
    /** @inheritdoc */
    def head = Some(value)

    /** @inheritdoc */
    def tail = next

    /** @inheritdoc */
    def toList = {
        @tailrec
        def recurse(acc: List[E], tail: MyList[E]): List[E] = tail match {
            case MyNil      => acc
            case Node(v, t) => recurse(acc ::: List(v), t)
        }

        recurse(Nil, this)
    }

    /**
     * Returns a nice string representation for this list.
     * So e.g. {{{MyList(1, 2, 3)}}} will be displayed as {{{[1, 2, 3]}}}.
     *
     * @return Returns the string representation.
     */
    override def toString = {
        @tailrec
        def recurse(str: String, ta: MyList[E]): String = ta match {
            case MyNil => str
            case Node(v, MyNil) => str + v
            case Node(v, t)     => recurse(str + v + ", ", t)
        }

        def wrap(l: MyList[E]): MyList[E] = l

        "[" + recurse("", wrap(this)) + "]"
    }
}

/**
 * Singleton object implementing the apply function for this list type.
 * 
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 */
object MyList {
    /**
     * Creates a new list with no entries (an empty list). To do so, call this
     * function via {{{MyList()}}}.
     *
     * @return Returns the empty list.
     */
    def apply[E](): MyList[E] = MyNil

    /**
     * Creates a new list with the specififed entries. To do so, call this 
     * function e.g. via {{{MyList(1, 2, 3)}}}.
     *
     * @param elem
     *         Head element of the list to create.
     * @param elems
     *         Variable number of elements to add to this list.
     * @return Returns the created list.
     */
    def apply[E](elem: E, elems: E*): MyList[E] = {
        @tailrec
        def recurse(elems: List[E], list: MyList[E]): MyList[E] = elems match {
            case Nil => list
            case _   => recurse(elems.tail, list + elems.head)
        }

        recurse(elems.toList, Node(elem, MyNil))
    }
}

