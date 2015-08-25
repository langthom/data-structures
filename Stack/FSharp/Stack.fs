(* Copyright (c) 2015, Thomas Lang. All rights reserved.
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
 *)

/// <summary>Implementation of a generic stack data structure.</summary>
/// A stack is a data structure where only the very first element is available.
/// This is an essential way to implement recursion.
/// 
/// @author Thomas Lang
/// @version 1.0, 2015-08-25
[<StructuredFormatDisplay("{AsString}")>]
type Stack<'a>() =
  let mutable stack = List.empty

  /// Override ToString to pretty-print.
  override x.ToString() =
    stack.ToString()

  member x.AsString = x.ToString()

  /// <summary>Checks if the stack is empty or not.</summary>
  /// <returns>Returns true if the stack is empty or false otherwise.</returns>
  member x.IsEmpty() =
    stack.IsEmpty

  /// <summary>Pushes the passed generic value onto this stack.</summary>
  /// <param name="value">The value to push onto the stack.</param>
  member x.Push (value: 'a) =
    stack <- value :: stack

  /// <summary>Deletes and returns the top element of the stack.</sumary>
  /// <returns>Returns the deleted element of the stack.</returns>
  member x.Pop() =
    match stack with
      | [] -> failwith "Cannot pop from empty stack."
      | _  ->
        let peek = List.head stack
        stack <- stack.Tail
        peek

  /// <summary>Returns the top element of the stack.</summary>
  /// <returns>Returns the top element of the stack.</summary>
  member x.Peek() =
    List.head stack
  
// Main (testing) function.
let main() =
  printf "Creating stack ... "
  let stack = Stack<int>()
  printfn "done."
  printf "Pushing '7', '42', '999' ... "
  stack.Push(7)
  stack.Push(42)
  stack.Push(999)
  printfn "done."
  printfn "Stack:"
  printfn "%A" stack
  printfn "Pop results: %d" (stack.Pop())
  printfn "Stack now:"
  printfn "%A" stack
  printfn "done."

main()
