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


# 📦 Instruções em Bytes

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
