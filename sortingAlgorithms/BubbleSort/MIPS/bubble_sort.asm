###########################################################################
#  Implementation of the Bubble Sort algorithm in MIPS assembler.
# (C) Prof. Dr. Ilia Polian, University of Passau, 2014					
#								
# The bubble sort algorithm runs over the passed array and swaps two nodes
# if they are not sorted. The main advantage of the bubble sort is that it
# can handle already sorted lists.					
# This algorithm runs in O(n^2).					
#
###########################################################################


.data

array:	.word	0:10			# declaring array of 10 integers


.text

	li	$t7,	4		# constant for computing offset
	li	$a1,	9		# size of array	
	la	$a0,	array		# loading array
	
	# initializing whole testing array
	li	$t0,	42
	sw	$t0,	0($a0)
	li	$t0,	0
	sw	$t0,	4($a0)
	li	$t0,	58
	sw	$t0,	8($a0)
	li	$t0,	723
	sw	$t0,	12($a0)
	li	$t0,	-13
	sw	$t0,	16($a0)
	li	$t0,	7
	sw	$t0,	20($a0)
	li	$t0,	999
	sw	$t0,	24($a0)
	li	$t0,	8
	sw	$t0,	28($a0)
	li	$t0,	21
	sw	$t0,	32($a0)

	jal	SORT
	j 	ALL_DONE


SORT:	addi	$sp,	$sp,	-20	# prepare stack
	sw	$ra,	16($sp)		# save return address
	sw	$s3,	12($sp)	
	sw	$s2,	8($sp)	
	sw 	$s1,	4($sp)
	sw	$s0,	0($sp)
	move	$s2,	$a0		# save $a0 into $s2
	move	$s3,	$a1
	move	$s0,	$zero		# i = 0
for1tst:	slt	$t0,	$s0,	$s3	# $t0 = ( i >= n ) ? 0 : 1
	beq	$t0,	$zero,	done1	# goto "done1" if $t0 == 0
	addi	$s1,	$s0,	-1	# j = i - 1
for2tst:	slti	$t0,	$s1,	0	# $t0 = ( j < 0 ) ? 1 : 0
	bne	$t0,	$zero,	done2	# goto "done2" if  j < 0
	sll	$t1,	$s1,	2	# $t1 = j * 4
	add	$t2,	$s2,	$t1	# $t2 = v + (j*4)
	lw	$t3,	0($t2)		# $t3 = v [ j ]
	lw	$t4,	4($t2)		# $t4 = v [ j + 1 ]
	slt	$t0,	$t4,	$t3	# $t0 = 0 if $t4 > $t3
	beq	$t0,	$zero,	done2	# goto "done2" if $t4 > $t3
	move	$a0,	$s2		# 1st param of swap is v
	move	$a1,	$s1		# 2nd param of swap is j
	jal 	swap			# call swap procedure
	addi	$s1,	$s1,	-1	# j--
	j	for2tst			# jump of inner loop
done2:	addi	$s0,	$s0,	1	# i++
	j	for1tst			# jump of outer loop
done1:	lw	$s0,	0($sp)		# restore $s0 from stack
	lw	$s1,	4($sp)		# restore $s1 from stack
	lw	$s2,	8($sp)		# restore $s2 from stack
	lw	$s3,	12($sp)		# restore $s3 from stack
	lw	$ra,	16($sp)		# restore $ra from stack
	addi	$sp,	$sp,	20	# restore stack (pop)
	jr 	$ra			# return to caller
	
swap:	sll	$t1,	$a1,	2	# $t1 = j * 4
	add	$t1,	$a0,	$t1	# $t1 = v + (j * 4)
	lw	$t0,	0($t1)		# $t0 (tmp) = v [ j ]
	lw	$t2,	4($t1)		# $t2 = v [ j + 1 ]
	sw	$t2,	0($t1)		# v [ j ] = v [ j + 1 ]
	sw	$t0,	4($t1)		# v [ j + 1 ] = tmp
	jr	$ra			# return to caller

ALL_DONE: