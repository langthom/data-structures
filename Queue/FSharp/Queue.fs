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

/// <summary>Implementation of a generic queue data structure.</summary>
/// A queue is a data structure where elements can be inserted at one end while
/// elements can only be removed from the opposite end.
/// 
/// @author Thomas Lang
/// @version 1.0, 2015-08-29
[<StructuredFormatDisplay("{AsString}")>]
type Queue<'a>() =
  let mutable queue = List.empty

  /// Override ToString to pretty-print.
  override x.ToString() =
    queue.ToString()

  member x.AsString = x.ToString()

  /// <summary>Checks if the queue is empty or not.</summary>
  /// <returns>Returns true if the queue is empty or false otherwise.</returns>
  member x.IsEmpty() =
    queue.IsEmpty

  /// <summary>Offeres the passed generic value onto this queue.</summary>
  /// <param name="value">The value to push onto the queue.</param>
  member x.Offer (value: 'a) =
    queue <- queue @ [value]

  /// <summary>Deletes and returns the top element of the queue.</sumary>
  /// <returns>Returns the deleted element of the queue.</returns>
  member x.Poll() =
    match queue with
      | [] -> failwith "Cannot pop from empty queue."
      | _  ->
        let peek = List.head queue
        queue <- queue.Tail
        peek

  /// <summary>Returns the top element of the queue.</summary>
  /// <returns>Returns the top element of the queue.</summary>
  member x.Peek() =
    List.head queue
  
// Main (testing) function.
let main() =
  printf "Creating queue ... "
  let queue = Queue<int>()
  printfn "done."
  printf "Offering '7', '42', '999' ... "
  queue.Offer(7)
  queue.Offer(42)
  queue.Offer(999)
  printfn "done."
  printfn "Queue:"
  printfn "%A" queue
  printfn "Poll results: %d" (queue.Poll())
  printfn "Queue now:"
  printfn "%A" queue
  printfn "done."

main()
