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
	subi $sp, $sp, 8
	li $t0, 50
	sw $t0, 4($fp)
	addi $t0, $fp, 4
	sw $t0, 8($fp)
	lw $t0, 8($fp)
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_char
	addi $sp, $sp, 4
	li $t0, 10
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_char
	addi $sp, $sp, 4
	lw $t0, 8($fp)
	li $t1, 51
	sw $t1, 0($t0)
	lw $t0, 4($fp)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	jal print_char
	addi $sp, $sp, 4
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
x:
	.word 0
z:
	.word 0
