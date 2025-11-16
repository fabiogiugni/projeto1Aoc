>Na interface do EDAPlayground, use as seguintes especificações:

L Languages & Libraries

    L Testbench + Design
        L VHDL

    L Top entity
        L MIPS_tb

L Tools & Simulators

    L GHDL 3.0.0

    L Import Options
        L -fsynopsys -fexplicit
    L Make Options
        L -fsynopsys -fexplicit
    L Run Options
        L -fsynopsys -fexplicit

>Para correção, as intruções (também disponíveis em Instruction_Memory são as seguintes)

# Set operands. In this example, multiplies 6x4
# So, in fact, this algorithm sums 6 times 4
    addi $s1, $0, 6
    addi $s2, $0, 4

# Set counters
    addi $s0, $0, 0
    addi $s3, $0, 0

for:
    beq $s0, $s1, continue
    add $s3, $s3, $s2
    addi $s0, $s0, 1
    j for

continue:
# The result is stored in $s3 memory position

# Test of operations from fp
    add.s $f2, $f0, $f1
    mul.s $f3, $f0, $f1

Divididas em Bytes:
x"20",x"11",x"00",x"06"
x"20",x"12",x"00",x"04"
x"20",x"10",x"00",x"00"
x"20",x"13",x"00",x"00"
x"12",x"11",x"00",x"03"
x"02",x"72",x"98",x"20"
x"22",x"10",x"00",x"01"
x"08",x"00",x"00",x"04"
x"46",x"01",x"00",x"80"
x"46",x"01",x"00",x"C2"

>Ainda para a correção(Atente-se aos valores finais):

R16 = 0x00000006 $s0
R17 = 0x00000006 $s1
R18 = 0x00000004 $s2
R19 = 0x00000018 $s3 (6*4 = 24)

F0 = 0x451C4000 $f0 +2500.0
F1 = 0xC4DAC000 $f1 -1750.0
F2 = 0x443B8000 $f2 +750.0
F3 = 0xCA8583B0 $f3 -4375000.0