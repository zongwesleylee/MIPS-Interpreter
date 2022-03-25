#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    //find opcode
    //make all false
    _Bool Jformat = 0;
    _Bool Rformat = 0;
    _Bool Iformat = 0;

    unsigned int opcode = instr >> 26;
    d->op = opcode;

    //find instruction format
    if (opcode == 2 || opcode == 3)
        Jformat = 1;
    else if (opcode == 0)
        Rformat = 1;
    else
        Iformat = 1;

    //format is R-type
    if (Rformat == 1) {
        d->type = R;
        rVals->R_rs = mips.registers[d->regs.r.rs = (instr & 0x03e00000) >> 21];
        rVals->R_rt = mips.registers[d->regs.r.rt = (instr & 0x001f0000) >> 16];
        rVals->R_rd = mips.registers[d->regs.r.rd = (instr & 0x0000f800) >> 11];
        d->regs.r.shamt = (instr & 0x000007c0) >> 6;
        d->regs.r.funct = instr & 0x0000003f;
    }

    //format is J-type
    if (Jformat == 1) {
        d->type = J;
        d->regs.j.target = instr & 0x03ffffff;
    }

    //format is I-type
    if (Iformat == 1) {
        d->type = I;
        rVals->R_rs = mips.registers[d->regs.i.rs = (instr & 0x03e00000) >> 21];
        rVals->R_rt = mips.registers[d->regs.i.rt = (instr & 0x001f0000) >> 16];
        d->regs.i.addr_or_immed = (short)(instr & 0x0000ffff);
    }
}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {

    //get the names of all instructions
    char *instructionName;
    if (d->regs.r.funct == 32)
        instructionName = "add";
    else if (d->op == 8)
        instructionName = "addi";
    else if (d->op == 9)
        instructionName = "addiu";
    else if (d->regs.r.funct == 33)
        instructionName = "addu";
    else if (d->regs.r.funct == 36)
        instructionName = "and";
    else if (d->op == 12)
        instructionName = "andi";
    else if (d->op == 4)
        instructionName = "beq";
    else if (d->op == 5)
        instructionName = "bne";
    else if (d->op == 2)
        instructionName = "j";
    else if (d->op == 3)
        instructionName = "jal";
    else if (d->regs.r.funct == 8)
        instructionName = "jr";
    else if (d->op == 36)
        instructionName = "lbu";
    else if (d->op == 37)
        instructionName = "lhu";
    else if (d->op == 48)
        instructionName = "ll";
    else if (d->op == 15)
        instructionName = "lui";
    else if (d->op == 35)
        instructionName = "lw";
    else if (d->regs.r.funct == 39)
        instructionName = "nor";
    else if (d->regs.r.funct == 37)
        instructionName = "or";
    else if (d->op == 13)
        instructionName = "ori";
    else if (d->regs.r.funct == 42)
        instructionName = "slt";
    else if (d->op == 10)
        instructionName = "slti";
    else if (d->op == 11)
        instructionName = "sltiu";
    else if (d->regs.r.funct == 43)
        instructionName = "sltu";
    else if (d->op == 0 && d->regs.r.funct == 0)
        instructionName = "sll";
    else if (d->regs.r.funct == 2)
        instructionName = "srl";
    else if (d->op == 40)
        instructionName = "sb";
    else if (d->op == 56)
        instructionName = "sc";
    else if (d->op == 41)
        instructionName = "sh";
    else if (d->op == 43)
        instructionName = "sw";
    else if (d->regs.r.funct == 34)
        instructionName = "sub";
    else if (d->regs.r.funct == 35)
        instructionName = "subu";

    int rd = d->regs.r.rd;
    int rs = d->regs.r.rs;
    int rt = d->regs.r.rt;
    short imm = d->regs.i.addr_or_immed;

    //if R-format
    if (d->type == R) {
        //jr function
        if (d->regs.r.funct == 8)
            printf("%s\t$%d\n", instructionName, rs);
            //srl and sll function
        else if (d->regs.r.funct == 0 || d->regs.r.funct == 2)
            printf("%s\t$%d, $%d, %d\n", instructionName, rd, rt, d->regs.r.shamt);
        else
            printf("%s\t$%d, $%d, $%d\n", instructionName, rd, rs, rt);
    }
    else if (d->type == J) {
        printf("%s\t0x%.8x\n", instructionName, (mips.pc & 0xf0000000) | ((d->regs.j.target << 2) & 0x0fffffff));
    }
    else {
        //bne, beq opcodes
        if (d->op == 5 || d->op == 4)
            printf("%s\t$%d, $%d, 0x%.8x\n", instructionName, rs, rt, mips.pc + 4 + (imm << 2));
        //addiu opcode
        else if (d->op == 9)
            printf("%s\t$%d, $%d, %d\n", instructionName, rt, rs, imm);
        //andi, ori, lui opcodes
        else if (d->op == 12 || d->op == 13 || d->op == 15)
            printf("%s\t$%d, $%d, 0x%x\n", instructionName, rt, rs, (unsigned short)imm);
        //lw and sw opcodes
        else if (d->op == 35 || d->op == 43)
            printf("%s\t$%d, %d($%d)\n", instructionName, rt, imm, rs);

    }
}

/* Perform computation needed to execute d, returning computed value */
int Execute(DecodedInstr* d, RegVals* rVals) {

    //get the ALU operation
    if (d->type == R) {
        //addu function
        if (d->regs.r.funct == 33)
            return rVals->R_rs + rVals->R_rt;
        //and function
        else if (d->regs.r.funct == 36)
            return rVals->R_rs & rVals->R_rt;
        //or function
        else if (d->regs.r.funct == 37)
            return rVals->R_rs | rVals->R_rt;
        //sll function
        else if (d->regs.r.funct == 0)
            return rVals->R_rt << d->regs.r.shamt;
        //slt function
        else if (d->regs.r.funct == 42)
            return rVals->R_rs < rVals->R_rt;
        //srl function
        else if (d->regs.r.funct == 2)
            return (unsigned int)rVals->R_rt >> d->regs.r.shamt;
        //subu function
        else if (d->regs.r.funct == 35)
            return rVals->R_rs - rVals->R_rt;
    }
    else if (d->type == I) {
        //addiu opcode
        if (d->op == 9)
            return rVals->R_rs + d->regs.i.addr_or_immed;
        //andi opcode
        else if (d->op == 12)
            return rVals->R_rs & d->regs.i.addr_or_immed;
         //beq opcode
        else if (d->op == 5)
            return rVals->R_rs == rVals->R_rt;
        //bne opcode
        else if (d->op == 4)
            return rVals->R_rs != rVals->R_rt;
        //lui opcode
        else if (d->op == 15)
            return d->regs.i.addr_or_immed << 16;
        //lw, sw opcodes
        else if (d->op == 35 || d->op == 43)
            return rVals->R_rs + (short)d->regs.i.addr_or_immed;
        //ori opcode
        else if (d->op == 13)
            return rVals->R_rs | d->regs.i.addr_or_immed;
    }
    else if (d->type == J) {
        //jal opcode
        if (d->op ==3)
            return mips.pc + 4;
    }

    return 0;
}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;
    
    //if R format and jr function
    if (d->type == R && d->regs.r.funct == 8) {
        mips.pc = mips.registers[d->regs.r.rs];
    }
    //beq opcode
    else if (d->op == 4) {
        if (val == 0)
            mips.pc = d->regs.i.addr_or_immed;
    }
    //bne opcode
    else if (d->op == 5) {
        if (val != 0)
            mips.pc = d->regs.i.addr_or_immed;
    }
    //jal opcode
    else if (d->op == 3) {
        mips.registers[31] = mips.pc;
        mips.pc = d->regs.j.target;
        return;
    }
    //j opcode
    else if (d->op == 2) {
        mips.pc = d->regs.j.target;
        return;
    }
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    //finds address
    int offset = d->regs.i.addr_or_immed;
    int address = offset + mips.registers[d->regs.i.rs];

    //converts address into index
    int index = (address - 0x00401000) / 4;

    //sw opcode
    if (d->op == 43) {
        mips.memory[index] = mips.registers[d->regs.i.rt];
        *changedMem = address;
    }
    //lw opcode
    else if (d->op == 35)
        return mips.memory[index];
    
    return val;
}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    if (d->type == I)
        *changedReg = d->regs.i.rt;
    else if (d->type == R)
        *changedReg = d->regs.r.rd;
    else
        *changedReg = 31;
    mips.registers[*changedReg] = val;
}
