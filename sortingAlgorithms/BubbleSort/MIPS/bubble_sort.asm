###########################################################################
#  Implementation of the Bubble Sort algorithm in MIPS assembler.
# (C) Thomas Lang, 2014/08/22					
#								
# The bubble sort algorithm runs over the passed array and swaps two nodes
# if they are not sorted. The main advantage of the bubble sort is that it
# can handle already sorted lists.					
# This algorithm runs in O(n^2).					
#									
# This code is licensed under the BSD3 license.				
###########################################################################

# note: until now this does not work fully,
# i will repair this soon

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


SORT:	addi	$sp,	$sp,	-4	# prepare stack
	sw	$ra,	0($sp)		# save return address
	
	li	$s0,	0		# int ready = 0;
	add	$s1,	$a1,	$zero	# int i = size;
	li	$s2,	0		# int j = 0;

L1:	beq	$s0,	1,	DONE	# first loop condition
	sgt	$t0,	$s1,	$zero	# second loop condition
	beq	$t0,	$zero,	DONE	
	li	$s0,	1		# set ready to 1
	
L2:	addi	$t0,	$s1,	-1	# $t0 = i - 1;
	slt	$t1,	$s2,	$t0	# $t1 = (j < i - 1) ? 1 : 0
	beq	$t1,	$zero,	DONE2	
	
	addi	$s3,	$s2,	1	# $s3 = j + 1
	mul	$s2,	$s2,	$t7	# computing offset for "j"
	add	$s2,	$a0,	$s2	# pointer adress to A[j]
	mul	$s3,	$s3,	$t7	# computing offset for "j+1"
	add	$s3,	$a0,	$s3	# pointer adress to A[j+1]
	lw	$t2,	($s2)		# write A[j] to $t2
	lw	$t3,	($s3)		# write A[j+1] to $t3
	sgt	$t4,	$t2,	$t3	# $t4 = (A[j] > A[j+1]) ? 1 : 0
	beq	$t4,	$zero,	NOPE	
	
	add	$t5,	$t2,	$zero	# int tmp = A[j];
	sw	$t3,	($s2)		# A[j] = A[j+1]
	sw	$t5,	($s3)		# A[j+1] = tmp
	
	li	$s0,	0		# set ready to 0
	
NOPE:	addi	$s2,	$s2,	1	# j++
	j	L2			# jump for looping in L2
	
DONE2:	addi	$s1,	$s1,	-1	# i--
	j	L1			# jump for looping in L1

DONE:					
	lw	$ra,	0($sp)		# return adress
	addi	$sp,	$sp,	4	# restoring stack
	
	jr	$ra			# final jump to caller
	
ALL_DONE:
