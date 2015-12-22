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
import mylist.MyList

/**
 * Testing module.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 */
object Main {
    def main(args: Array[String]) {
        val l1 = MyList(1, 2, 3)
        println("Initial list: " + l1)

        val l2 = l1 + 7
        println("add(7): " + l2)

        val l22 = l2.addFront(999)
        println("addFront(999): " + l22)

        val l3 = l22 - 2
        println("remove(2): " + l3)

        val l4 = l3.clear() // match { case None => MyList(); case Some(l) => l.clear() }
        println("clear(): " + l4)
    }
}

