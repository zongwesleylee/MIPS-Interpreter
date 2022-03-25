addiu	$4,$0,3
addiu	$5,$0,2
jal	Mystery
sll 	$0, $0, 0
srl	$1, $1, 5
addi	$0,$0,0
lui 	$1, 0x0100
addiu	$1, $1, 0x0400
jr 	$3
lui	$2, 0xffff


Mystery:
addiu	$2,$0,0
Loop:
beq	$4,$0,Done
addu	$2,$2,$5
addiu	$4,$4,-1
j Loop
Done:	
jr	$31
