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
! Implementation of a simple generic stack in Fortran90.
! ------------------------------------------------------------------------------
! Compiled with:
!    gfortran -o stacktest stack.f90
!
! Tested on Linux.
! ------------------------------------------------------------------------------

! Implementation of a single data holding node.
! Author: Thomas Lang
! Version: 2015-08-25
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

! Implementation of a simple stack that holds some generic nodes that hold data.
! Author: Thomas Lang
! Version: 2015-08-25
MODULE Gen_Stack
  IMPLICIT NONE

  PRIVATE
  PUBLIC :: stack_node_t, stack_data
  PUBLIC :: stack_init, stack_free
  PUBLIC :: stack_push, stack_pop, stack_peek

  ! Holding a public value that can be transferred easily.
  INTEGER, DIMENSION(:), ALLOCATABLE :: stack_data

  ! Type of an inner node.
  ! This holds a pointer to a real node
  ! and a reference to its immediate successor.
  TYPE :: stack_node_t
    PRIVATE
    INTEGER, DIMENSION(:), POINTER :: data => null()
    TYPE(stack_node_t), POINTER :: next => null()
  END TYPE stack_node_t

CONTAINS

  ! Creates a new stack by allocating a very first node containing the passed
  ! data.
  SUBROUTINE stack_init(self, data)
    TYPE(stack_node_t), POINTER :: self
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: data

    ! Allocate an empty node.
    ALLOCATE(self)
    NULLIFY(self%next)

    ! If successfully allocated, allocate internal data and set it.
    ! If not, nullify allocated pointer and exit.
    IF (PRESENT(data)) THEN
      ALLOCATE(self%data(SIZE(data)))
      self%data = data
    ELSE
      NULLIFY(self%data)
    END IF
  END SUBROUTINE stack_init

  ! Free the entire stack beginning at SELF
  SUBROUTINE stack_free(self)
    TYPE(stack_node_t), POINTER :: self
    TYPE(stack_node_t), POINTER :: current
    TYPE(stack_node_t), POINTER :: next

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
  END SUBROUTINE stack_free

  ! Pushes the passed data onto the stack.
  SUBROUTINE stack_push(self, data)
    TYPE(stack_node_t), POINTER :: self
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: data
    TYPE(stack_node_t), POINTER :: next
    TYPE(stack_node_t), POINTER :: selfcopy

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
    self => next
  END SUBROUTINE stack_push

  ! Deletes and returns the top element of the stack.
  SUBROUTINE stack_pop(self)
    TYPE(stack_node_t), POINTER :: self
    TYPE(stack_node_t), POINTER :: current
    TYPE(stack_node_t), POINTER :: next

    ! Setting stack_data to self%data to make this value easily transferble
    ! for usage in other ways ...
    stack_data = self%data

    next => self%next

    IF (ASSOCIATED(self%data)) THEN
      DEALLOCATE(self%data)
      NULLIFY(self%data)
    END IF
    DEALLOCATE(self)
    NULLIFY(self)
    self => next
  END SUBROUTINE stack_pop

  ! Gets the top value of the stack.
  SUBROUTINE stack_peek(self)
    TYPE(stack_node_t), POINTER :: self
    stack_data = self%data
  END SUBROUTINE stack_peek
END MODULE Gen_Stack

! Main program, testing.
! Author: Thomas Lang
! Version: 2015-08-25
PROGRAM StackTest
  USE Gen_Stack
  USE Node
  IMPLICIT NONE

  TYPE(stack_node_t), POINTER :: stack => null()
  TYPE(node_ptr) :: ptr
  TYPE(node_ptr) :: popvalue

  ! Allocate new data element
  ALLOCATE(ptr%point)
  ptr%point%value = 3.1415

  ! Initialize Stack
  CALL stack_init(stack, TRANSFER(ptr, stack_data))
  PRINT *, 'Initializing stack with: ', ptr%point

  ALLOCATE(ptr%point)
  ptr%point%value = 1.4142

  ! Push second element onto stack
  CALL stack_push(stack, TRANSFER(ptr, stack_data))
  PRINT *, 'Pushed element: ', ptr%point

  ! Pop value
  CALL stack_pop(stack)
  popvalue = TRANSFER(stack_data, popvalue)
  PRINT *, 'Pop returned: ', popvalue%point%value

  ! Free head node.
  CALL stack_peek(stack)
  ptr = TRANSFER(stack_data, ptr)
  PRINT *, 'Peek returned: ', ptr%point%value
  DEALLOCATE(ptr%point)

  ! Free the stack
  CALL stack_free(stack)  
END PROGRAM StackTest
