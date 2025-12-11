# Configuração do Projeto no EDAPlayground

Para simular este projeto no **EDAPlayground**, utilize as seguintes configurações:

---

## 🔧 Languages & Libraries

- **Testbench + Design**
  - VHDL

- **Top entity**
  - `MIPS_tb`

---

## 🛠️ Tools & Simulators

- **GHDL 3.0.0**

### Import Options

-fsynopsys -fexplicit

### Make Options

-fsynopsys -fexplicit

### Run Options

-fsynopsys -fexplicit


---

# 📜 Instruções para Correção

As instruções abaixo (também disponíveis em `Instruction_Memory`) são utilizadas para validação do funcionamento:

```asm
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


📦 Instruções em Bytes

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

✍️ Estado final dos registradores (Parte que é importante para análise para análise)

(...)

O estado final dos registradores confirma o êxito das operações testadas.

testbench.vhd:137:9:@1035ns:(report note): === ESTADO FINAL DOS REGISTRADORES (via probe inteiro) ===
testbench.vhd:141:13:@1052ns:(report note): R16 = 0x00000006    $s0     (6)dec
testbench.vhd:141:13:@1053ns:(report note): R17 = 0x00000006    $s1     (6)dec
testbench.vhd:141:13:@1054ns:(report note): R18 = 0x00000004    $s2     (4)dec
testbench.vhd:141:13:@1055ns:(report note): R19 = 0x00000018    $s3     (24)dec   ($s1 * $s2)
testbench.vhd:145:9:@1067ns:(report note): === ESTADO FINAL DOS REGISTRADORES DE PONTO FLUTUANTE (via FP probe) ===
testbench.vhd:149:13:@1068ns:(report note): F0 = 0x451C4000     $f0     (+2500.0)dec 
testbench.vhd:149:13:@1069ns:(report note): F1 = 0xC4DAC000     $f1     (-1750.0)dec
testbench.vhd:149:13:@1070ns:(report note): F2 = 0x443B8000     $f2     (+749.0)dec   ($f0 + $f1)
testbench.vhd:149:13:@1071ns:(report note): F3 = 0xCA8583B0     $f3     (-4 283 520.0)dec   ($f0 * $f1)

Link para projeto no EDAPlayground: https://edaplayground.com/x/uiYJ
