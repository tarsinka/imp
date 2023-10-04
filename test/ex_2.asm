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
	subi $sp, $sp, 12
	li $t0, 5
	sw $t0, 4($fp)
	li $t0, 2
	sw $t0, 8($fp)
	li $t0, 2
	lw $t1, 4($fp)
	mul $t0, $t0, $t1
	lw $t1, 8($fp)
	add $t0, $t0, $t1
	li $t1, 5
	add $t0, $t0, $t1
	sw $t0, 12($fp)
	lw $t0, 12($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_int
	addi $sp, $sp, 4
	lw $t0, 4($fp)
	lw $t1, 8($fp)
	mul $t0, $t0, $t1
	sw $t0, 12($fp)
	b __main_0
__main_1:
	lw $t0, 4($fp)
	li $t1, 1
	add $t0, $t0, $t1
	sw $t0, 4($fp)
	lw $t0, 8($fp)
	lw $t1, 4($fp)
	mul $t0, $t0, $t1
	sw $t0, 8($fp)
__main_0:
	lw $t0, 4($fp)
	li $t1, 5
	seq $t0, $t0, $t1
	bnez $t0, __main_1
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
