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

import bintree.BinarySearchTree

/**
 * Testing module.
 *
 * @author Thomas Lang
 * @version 1.0, 2015-12-22
 */
object Main {
    def main(args: Array[String]) {
        val tree = BinarySearchTree(1, 2, 3)
        println("Tree: " + tree)

        val t2 = tree + 7
        println("insert(7): " + t2)

        val t3 = t2 + 0
        println("insert(0): " + t3)

        val t4 = t3 - 2
        println("remove(2): " + t4)
    }
}

