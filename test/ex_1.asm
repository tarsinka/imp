.text
	subi $sp, $sp, 4
	sw $a0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	subi $sp, $sp, 16
	li $t0, 0
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 33
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 100
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 108
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 114
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 111
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 87
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 32
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 44
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 111
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 108
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 108
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 101
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 72
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	move $t0, $sp
	sw $t0, -20($fp)
	lw $t0, -20($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print
	addi $sp, $sp, 4
	li $t0, 1
	sw $t0, -12($fp)
	li $t0, 5
	sw $t0, -8($fp)
	lw $t0, -8($fp)
	li $t1, 5
	slt $t0, $t0, $t1
	bnez $t0, __main_0
	lw $t0, -8($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_int
	addi $sp, $sp, 4
	b __main_1
__main_0:
	lw $t0, -20($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print
	addi $sp, $sp, 4
__main_1:
	addi $t0, $fp, -8
	sw $t0, -16($fp)
	lw $t0, -16($fp)
	lw $t1, -8($fp)
	li $t2, 6
	mul $t1, $t1, $t2
	sw $t1, 0($t0)
	lw $t0, -16($fp)
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_int
	addi $sp, $sp, 4
	lw $t0, -8($fp)
	subi $sp, $fp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
	li $t0, 0
	subi $sp, $fp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
print:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	subi $sp, $sp, 8
	li $t0, 97
	sw $t0, -8($fp)
	li $t0, -1
	sw $t0, -12($fp)
	b __print_0
__print_1:
	lw $t0, -12($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, -12($fp)
	li $t0, 4
	lw $t1, -12($fp)
	mul $t0, $t0, $t1
	lw $t1, 4($fp)
	add $t0, $t1, $t0
	lw $t0, 0($t0)
	sw $t0, -8($fp)
	lw $t0, -8($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_char
	addi $sp, $sp, 4
__print_0:
	lw $t0, -12($fp)
	li $t1, -1
	seq $t0, $t0, $t1
	lw $t1, -8($fp)
	or $t0, $t0, $t1
	li $t1, 0
	sgt $t0, $t0, $t1
	bnez $t0, __print_1
	li $t0, 0
	subi $sp, $fp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
print_int:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	lw $a0, 4($fp)
	li $v0, 1
	syscall
	subi $sp, $fp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
print_char:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	lw $a0, 4($fp)
	li $v0, 11
	syscall
	subi $sp, $fp, 4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
.data
a:
	.word 0
