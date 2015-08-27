! Copyright (c) 2015, Thomas Lang. All rights reserved.
! 
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
! 
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
! 
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <http://www.gnu.org/licenses/>
! ------------------------------------------------------------------------------
! Implementation of a simple generic queue in Fortran90.
! ------------------------------------------------------------------------------
! Compiled with:
!    gfortran -o queuetest queue.f90
!
! Tested on Linux.
! ------------------------------------------------------------------------------

! Implementation of a single data holding node.
! Author: Thomas Lang
! Version: 2015-08-27
MODULE Node
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: node_t
  PUBLIC :: node_ptr

  ! Value is hold in node_t
  TYPE :: node_t
    REAL :: value
  END TYPE node_t

  ! Container for storing node_t pointers.
  TYPE :: node_ptr
    TYPE(node_t), POINTER :: point
  END TYPE node_ptr
END MODULE Node

! Implementation of a simple queue that holds some generic nodes that hold data.
! Author: Thomas Lang
! Version: 2015-08-25
MODULE Gen_Stack
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: queue_node_t, queue_data
  PUBLIC :: queue_init, queue_free
  PUBLIC :: queue_offer, queue_poll, queue_peek

  ! Holding a public value that can be transferred easily.
  INTEGER, DIMENSION(:), ALLOCATABLE :: queue_data

  ! Type of an inner node.
  ! This holds a pointer to a real node
  ! and a reference to its immediate successor.
  TYPE :: queue_node_t
    PRIVATE
    INTEGER, DIMENSION(:), POINTER :: data => null()
    TYPE(queue_node_t), POINTER :: prev => null()
    TYPE(queue_node_t), POINTER :: next => null()
  END TYPE queue_node_t

CONTAINS

  ! Creates a new queue by allocating a very first node containing the passed
  ! data.
  SUBROUTINE queue_init(self, data)
    TYPE(queue_node_t), POINTER :: self
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: data

    ! Allocate an empty node.
    ALLOCATE(self)
    NULLIFY(self%prev)
    NULLIFY(self%next)

    ! If successfully allocated, allocate internal data and set it.
    ! If not, nullify allocated pointer and exit.
    IF (PRESENT(data)) THEN
      ALLOCATE(self%data(SIZE(data)))
      self%data = data
    ELSE
      NULLIFY(self%data)
    END IF
  END SUBROUTINE queue_init

  ! Free the entire queue beginning at SELF
  SUBROUTINE queue_free(self)
    TYPE(queue_node_t), POINTER :: self
    TYPE(queue_node_t), POINTER :: current
    TYPE(queue_node_t), POINTER :: next

    ! First, start at self.
    ! If there is something associated, deallocate the internal saved data and
    ! set the 'next' pointer to null. Then deallocate and nullify the current
    ! node and proceed with the next one, until every node is freed.
    current => self
    DO WHILE (ASSOCIATED(current))
      next => current%next
      IF (ASSOCIATED(current%data)) THEN
        DEALLOCATE(current%data)
        NULLIFY(current%data)
      END IF
      DEALLOCATE(current)
      NULLIFY(current)
      current => next
    END DO
  END SUBROUTINE queue_free

  ! Pushes the passed data onto the queue.
  SUBROUTINE queue_offer(self, data)
    TYPE(queue_node_t), POINTER :: self
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: data
    TYPE(queue_node_t), POINTER :: next
    TYPE(queue_node_t), POINTER :: selfcopy

    selfcopy => self

    ! Allocate an empty node.
    ALLOCATE(next)

    ! If successfully allocated, allocate internal data and set it.
    ! If not, nullify allocated pointer and exit.
    IF (PRESENT(data)) THEN
      ALLOCATE(next%data(SIZE(data)))
      next%data = data
    ELSE
      NULLIFY(next%data)
    END IF

    next%next => self
    self%prev => next
    self => next
  END SUBROUTINE queue_offer

  ! Deletes and returns the last element of the queue.
  SUBROUTINE queue_poll(self)
    TYPE(queue_node_t), POINTER :: self
    TYPE(queue_node_t), POINTER :: current
    TYPE(queue_node_t), POINTER :: next

    current => self
    DO WHILE (ASSOCIATED(current%next))
      current => current%next
    END DO

    ! Setting queue_data to self%data to make this value easily transferble
    ! for usage in other ways ...
    queue_data = current%data

    IF (ASSOCIATED(current%data)) THEN
        DEALLOCATE(current%data)
        NULLIFY(current%data)
    END IF
    
    NULLIFY(current%prev%next)
    NULLIFY(current%prev)
    DEALLOCATE(current)
    NULLIFY(current)
  END SUBROUTINE queue_poll

  ! Gets the last value of the queue.
  SUBROUTINE queue_peek(self)
    TYPE(queue_node_t), POINTER :: self
    TYPE(queue_node_t), POINTER :: selfcopy

    ! Move to end of queue.
    selfcopy => self
    DO WHILE (ASSOCIATED(selfcopy%next))
      selfcopy => selfcopy%next
    END DO

    queue_data = selfcopy%data
  END SUBROUTINE queue_peek
END MODULE Gen_Stack

! Main program, testing.
! Author: Thomas Lang
! Version: 2015-08-27
PROGRAM StackTest
  USE Gen_Stack
  USE Node
  IMPLICIT NONE

  TYPE(queue_node_t), POINTER :: queue => null()
  TYPE(node_ptr) :: ptr
  TYPE(node_ptr) :: pollvalue

  ! Allocate new data element
  ALLOCATE(ptr%point)
  ptr%point%value = 3.1415

  ! Initialize Stack
  CALL queue_init(queue, TRANSFER(ptr, queue_data))
  PRINT *, 'Initializing queue with: ', ptr%point

  ALLOCATE(ptr%point)
  ptr%point%value = 1.4142

  ! Push second element onto queue
  CALL queue_offer(queue, TRANSFER(ptr, queue_data))
  PRINT *, 'Offered element: ', ptr%point

  ! Pop value
  CALL queue_poll(queue)
  pollvalue = TRANSFER(queue_data, pollvalue)
  PRINT *, 'Poll returned: ', pollvalue%point%value

  ! Free head node.
  CALL queue_peek(queue)
  ptr = TRANSFER(queue_data, ptr)
  PRINT *, 'Peek returned: ', ptr%point%value
  DEALLOCATE(ptr%point)

  ! Free the queue
  CALL queue_free(queue)  
END PROGRAM StackTest
