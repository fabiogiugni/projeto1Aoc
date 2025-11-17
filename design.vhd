-- PC.vhd (corrigido: usa um sinal interno pc_reg)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity PC is
    Port (
        clk   : in  STD_LOGIC;
        reset : in  STD_LOGIC;
        stall : in  STD_LOGIC;
        din   : in  STD_LOGIC_VECTOR (31 downto 0);
        dout  : out STD_LOGIC_VECTOR (31 downto 0)
    );
end PC;

architecture Behavioral of PC is
    signal pc_reg : STD_LOGIC_VECTOR (31 downto 0) := (others => '0');
begin
    -- drive the output from the internal register
    dout <= pc_reg;
    process(clk, reset)
    begin
        if reset = '1' then
            pc_reg <= (others => '0');
        elsif rising_edge(clk) then
            if stall = '0' then
                pc_reg <= din;
            else
                -- nada a ler/escrever em dout: mantém pc_reg
                -- pc_reg <= pc_reg;  -- opcional e redundante
            end if;
        end if;
    end process;
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Instruction_Memory is
    Port ( dir : in  STD_LOGIC_VECTOR (31 downto 0);
           instr : out  STD_LOGIC_VECTOR (31 downto 0));
end Instruction_Memory;

architecture Behavioral of Instruction_Memory is

    type mem is array(0 to 511) of std_logic_vector(7 downto 0);
    		-- Load here your software.

--		Example code:
--		# Set operands. In this example, multiplies 6x4
--		# So, in fact, this algorithm sums 6 times 4
--		addi $s1, $0, 6
--		addi $s2, $0, 4
--
--		# Set counters
--		addi $s0, $0, 0
--		addi $s3, $0, 0
--
--		for:
--		beq $s0, $s1, continue
--		  add $s3, $s3, $s2
--		  addi $s0, $s0, 1
--		  j for
--		continue:
--		# The result is stored in $s3 memory position
--		  add.s $f2, $f0, $f1
--		  mul.s $f3, $f0, $f1

    constant code : mem := (
        x"20",x"11",x"00",x"06",
        x"20",x"12",x"00",x"04",
        x"20",x"10",x"00",x"00",
        x"20",x"13",x"00",x"00",
        x"12",x"11",x"00",x"03",
        x"02",x"72",x"98",x"20",
        x"22",x"10",x"00",x"01",
        x"08",x"00",x"00",x"04",
        x"46",x"01",x"00",x"80",
        x"46",x"01",x"00",x"C2",

        others => x"00"
    );

    -- checa se vetor tem X/Z
    function has_unknown(v : std_logic_vector) return boolean is
    begin
        for i in v'range loop
            if v(i) = 'X' or v(i) = 'Z' then
                return true;
            end if;
        end loop;
        return false;
    end function;

begin

    process(dir)
        variable idx : integer;
        variable low9 : unsigned(8 downto 0);
    begin
        if has_unknown(dir) then
            -- endereço indefinido: devolve NOP (instrução 0)
            instr <= (others => '0');
            report "Instruction_Memory: dir had unknown bits, returning NOP" severity warning;
        else
            -- use só os 9 LSB do endereço (0..511) para indexar bytes
            low9 := unsigned(dir(8 downto 0));
            idx := to_integer(low9);  -- 0..511 safe

            -- lê 4 bytes de forma segura (wrap-around com mod 512)
            instr(31 downto 24) <= code((idx + 0) mod 512);
            instr(23 downto 16) <= code((idx + 1) mod 512);
            instr(15 downto 8)  <= code((idx + 2) mod 512);
            instr(7 downto 0)   <= code((idx + 3) mod 512);
        end if;
    end process;

end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity MainDecoder is
    Port ( opcode : in  STD_LOGIC_VECTOR(5 downto 0);
           RegWrite : out  STD_LOGIC;
           RegDst : out  STD_LOGIC;
           ALUSrc : out  STD_LOGIC;
           Branch : out  STD_LOGIC;
           MemWrite : out  STD_LOGIC;
           MemtoReg : out  STD_LOGIC;
           ALUOp : out  STD_LOGIC_VECTOR(1 downto 0);
           Jump : out  STD_LOGIC);
end MainDecoder;

architecture Behavioral of MainDecoder is
 signal controls: std_logic_vector(8 downto 0);
begin

process(opcode)
	begin
	case opcode is
		when "000000" => controls <= "110000010";
		when "100011" => controls <= "101001000";
		when "101011" => controls <= "001010000";
		when "000100" => controls <= "000100001";
		when "001000" => controls <= "101000000";
		when "000010" => controls <= "000000100";
        when "010001" => controls <= "000000000";
		when others   => controls <= (others => '0');
	end case;
	end process;
	
	regwrite	<=controls(8);
	regdst	<=controls(7);
	alusrc	<=controls(6);
	branch	<=controls(5);
	memwrite	<=controls(4);
	memtoreg	<=controls(3);
	jump		<=controls(2);
	aluop		<=controls(1 downto 0);
	
	
end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity ALUdecoder is
    Port ( ALUop : in  STD_LOGIC_VECTOR (1 downto 0);
           funct : in  STD_LOGIC_VECTOR (5 downto 0);
           ALUctrl : out  STD_LOGIC_VECTOR (2 downto 0));
end ALUdecoder;

architecture Behavioral of ALUdecoder is

begin

	process(aluop,funct)
	begin
		case aluop is
			when "00" => aluctrl <="010";
			when "01" => aluctrl	<="110";
			when others => case funct is 
				when "100000" => aluctrl <= "010";
				when "100010" => aluctrl <= "110";
				when "100100" => aluctrl <= "000";
				when "100101" => aluctrl <= "001";
				when "101010" => aluctrl <= "111";
				when others   => aluctrl <= (others => 'X');
			end case;
		end case;
	end process;
				
end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity ControlUnit is
    Port ( OpCode : in  STD_LOGIC_VECTOR (5 downto 0);
           Funct : in  STD_LOGIC_VECTOR (5 downto 0);
           MemtoReg : out  STD_LOGIC;
           MemWrite : out  STD_LOGIC;
           Branch : out  STD_LOGIC;
           AluSrc : out  STD_LOGIC;
           RegDst : out  STD_LOGIC;
           RegWrite : out  STD_LOGIC;
			  jump : out std_logic;
           AluCtrl : out  STD_LOGIC_VECTOR (2 downto 0));
end ControlUnit;

architecture Behavioral of ControlUnit is
	COMPONENT MainDecoder
	PORT(
		opcode : IN std_logic_vector(5 downto 0);          
		RegWrite : OUT std_logic;
		RegDst : OUT std_logic;
		ALUSrc : OUT std_logic;
		Branch : OUT std_logic;
		MemWrite : OUT std_logic;
		MemtoReg : OUT std_logic;
		ALUOp : OUT std_logic_vector(1 downto 0);
		Jump : OUT std_logic
		);
	END COMPONENT;
	
	COMPONENT ALUdecoder
	PORT(
		ALUop : IN std_logic_vector(1 downto 0);
		funct : IN std_logic_vector(5 downto 0);          
		ALUctrl : OUT std_logic_vector(2 downto 0)
		);
	END COMPONENT;

	signal opalu:std_logic_vector(1 downto 0);

begin

Inst_MainDecoder: MainDecoder PORT MAP(
		opcode => opcode,
		RegWrite => regwrite,
		RegDst => regdst,
		ALUSrc => alusrc ,
		Branch => branch,
		MemWrite => memwrite,
		MemtoReg => memtoreg,
		ALUOp => opalu,
		Jump => jump
	);

Inst_ALUdecoder: ALUdecoder PORT MAP(
		ALUop => opalu,
		funct => funct,
		ALUctrl => aluctrl
	);
	
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity Register_File is
    Port (
        clk : in  STD_LOGIC;
        we3 : in  STD_LOGIC;
        A1  : in  STD_LOGIC_VECTOR (4 downto 0);
        A2  : in  STD_LOGIC_VECTOR (4 downto 0);
        A3  : in  STD_LOGIC_VECTOR (4 downto 0);
        RD1 : out STD_LOGIC_VECTOR (31 downto 0);
        RD2 : out STD_LOGIC_VECTOR (31 downto 0);
        WD3 : in  STD_LOGIC_VECTOR (31 downto 0);
        -- debug probe
        probe_index : in  STD_LOGIC_VECTOR(4 downto 0);
        probe_data  : out STD_LOGIC_VECTOR(31 downto 0)
    );
end Register_File;

architecture Behavioral of Register_File is

    type ram_type is array(0 to 31) of std_logic_vector(31 downto 0);
    -- inicializa todos os registradores com zero
    signal ram : ram_type := (others => (others => '0'));

begin

    -- escrita síncrona (mantém x0 sempre zero: não escreve no endereço 0)
    process(clk)
    begin
        if rising_edge(clk) then
            if we3 = '1' then
                if to_integer(unsigned(A3)) /= 0 then
                    ram(to_integer(unsigned(A3))) <= WD3;
                end if;
            end if;
        end if;
    end process;

    -- leitura combinacional (x0 sempre zero)
    process(A1, A2, ram)
    begin
        if to_integer(unsigned(A1)) = 0 then
            RD1 <= (others => '0');
        else
            RD1 <= ram(to_integer(unsigned(A1)));
        end if;

        if to_integer(unsigned(A2)) = 0 then
            RD2 <= (others => '0');
        else
            RD2 <= ram(to_integer(unsigned(A2)));
        end if;
    end process;

    -- probe combinacional para depuração
    process(probe_index, ram)
        variable idx : integer;
    begin
        idx := to_integer(unsigned(probe_index));
        if idx >= 0 and idx <= 31 then
            probe_data <= ram(idx);
        else
            probe_data <= (others => 'X');
        end if;
    end process;

end Behavioral;

--------------------------------------------------------------------------------
--                  RightShifterSticky26_by_max_25_comb_uid4
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X S
-- Output signals: R Sticky
--  approx. input signal timings: X: 2.250000nsS: 4.360000ns
--  approx. output signal timings: R: 5.460000nsSticky: 7.750000ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity RightShifterSticky26_by_max_25_comb_uid4 is
    port (X : in  std_logic_vector(25 downto 0);
          S : in  std_logic_vector(4 downto 0);
          R : out  std_logic_vector(25 downto 0);
          Sticky : out  std_logic   );
end entity;

architecture arch of RightShifterSticky26_by_max_25_comb_uid4 is
signal ps :  std_logic_vector(4 downto 0);
   -- timing of ps: 4.360000ns
signal Xpadded :  std_logic_vector(25 downto 0);
   -- timing of Xpadded: 2.250000ns
signal level5 :  std_logic_vector(25 downto 0);
   -- timing of level5: 2.250000ns
signal stk4 :  std_logic;
   -- timing of stk4: 4.950000ns
signal level4 :  std_logic_vector(25 downto 0);
   -- timing of level4: 4.360000ns
signal stk3 :  std_logic;
   -- timing of stk3: 5.520000ns
signal level3 :  std_logic_vector(25 downto 0);
   -- timing of level3: 4.910000ns
signal stk2 :  std_logic;
   -- timing of stk2: 6.080000ns
signal level2 :  std_logic_vector(25 downto 0);
   -- timing of level2: 4.910000ns
signal stk1 :  std_logic;
   -- timing of stk1: 6.640000ns
signal level1 :  std_logic_vector(25 downto 0);
   -- timing of level1: 5.460000ns
signal stk0 :  std_logic;
   -- timing of stk0: 7.200000ns
signal level0 :  std_logic_vector(25 downto 0);
   -- timing of level0: 5.460000ns
signal stk :  std_logic;
   -- timing of stk: 7.750000ns
begin
   ps<= S;
   Xpadded <= X;
   level5<= Xpadded;
   stk4 <= '1' when (level5(15 downto 0)/="0000000000000000" and ps(4)='1')   else '0';
   level4 <=  level5 when  ps(4)='0'    else (15 downto 0 => '0') & level5(25 downto 16);
   stk3 <= '1' when (level4(7 downto 0)/="00000000" and ps(3)='1') or stk4 ='1'   else '0';
   level3 <=  level4 when  ps(3)='0'    else (7 downto 0 => '0') & level4(25 downto 8);
   stk2 <= '1' when (level3(3 downto 0)/="0000" and ps(2)='1') or stk3 ='1'   else '0';
   level2 <=  level3 when  ps(2)='0'    else (3 downto 0 => '0') & level3(25 downto 4);
   stk1 <= '1' when (level2(1 downto 0)/="00" and ps(1)='1') or stk2 ='1'   else '0';
   level1 <=  level2 when  ps(1)='0'    else (1 downto 0 => '0') & level2(25 downto 2);
   stk0 <= '1' when (level1(0 downto 0)/="0" and ps(0)='1') or stk1 ='1'   else '0';
   level0 <=  level1 when  ps(0)='0'    else (0 downto 0 => '0') & level1(25 downto 1);
   stk <= stk0;
   R <= level0;
   Sticky <= stk;
end architecture;

--------------------------------------------------------------------------------
--                           IntAdder_27_comb_uid6
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: 2.250000nsY: 6.010000nsCin: 7.750000ns
--  approx. output signal timings: R: 9.010000ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_27_comb_uid6 is
    port (X : in  std_logic_vector(26 downto 0);
          Y : in  std_logic_vector(26 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(26 downto 0)   );
end entity;

architecture arch of IntAdder_27_comb_uid6 is
signal Rtmp :  std_logic_vector(26 downto 0);
   -- timing of Rtmp: 9.010000ns
begin
   Rtmp <= X + Y + Cin;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                              LZC_26_comb_uid8
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Bogdan Pasca (2007)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: I
-- Output signals: O
--  approx. input signal timings: I: 9.010000ns
--  approx. output signal timings: O: 12.930000ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LZC_26_comb_uid8 is
    port (I : in  std_logic_vector(25 downto 0);
          O : out  std_logic_vector(4 downto 0)   );
end entity;

architecture arch of LZC_26_comb_uid8 is
signal level5 :  std_logic_vector(30 downto 0);
   -- timing of level5: 9.010000ns
signal digit4 :  std_logic;
   -- timing of digit4: 9.600000ns
signal level4 :  std_logic_vector(14 downto 0);
   -- timing of level4: 10.150000ns
signal digit3 :  std_logic;
   -- timing of digit3: 10.720000ns
signal level3 :  std_logic_vector(6 downto 0);
   -- timing of level3: 11.270000ns
signal digit2 :  std_logic;
   -- timing of digit2: 11.830000ns
signal level2 :  std_logic_vector(2 downto 0);
   -- timing of level2: 12.380000ns
signal lowBits :  std_logic_vector(1 downto 0);
   -- timing of lowBits: 12.930000ns
signal outHighBits :  std_logic_vector(2 downto 0);
   -- timing of outHighBits: 11.830000ns
begin
   -- pad input to the next power of two minus 1
   level5 <= I & "11111";
   -- Main iteration for large inputs
   digit4<= '1' when level5(30 downto 15) = "0000000000000000" else '0';
   level4<= level5(14 downto 0) when digit4='1' else level5(30 downto 16);
   digit3<= '1' when level4(14 downto 7) = "00000000" else '0';
   level3<= level4(6 downto 0) when digit3='1' else level4(14 downto 8);
   digit2<= '1' when level3(6 downto 3) = "0000" else '0';
   level2<= level3(2 downto 0) when digit2='1' else level3(6 downto 4);
   -- Finish counting with one LUT
   with level2  select  lowBits <= 
      "11" when "000",
      "10" when "001",
      "01" when "010",
      "01" when "011",
      "00" when others;
   outHighBits <= digit4 & digit3 & digit2 & "";
   O <= outHighBits & lowBits ;
end architecture;

--------------------------------------------------------------------------------
--                     LeftShifter27_by_max_26_comb_uid10
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca (2008-2011), Florent de Dinechin (2008-2019)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X S
-- Output signals: R
--  approx. input signal timings: X: 9.010000nsS: 14.520000ns
--  approx. output signal timings: R: 16.727692ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity LeftShifter27_by_max_26_comb_uid10 is
    port (X : in  std_logic_vector(26 downto 0);
          S : in  std_logic_vector(4 downto 0);
          R : out  std_logic_vector(52 downto 0)   );
end entity;

architecture arch of LeftShifter27_by_max_26_comb_uid10 is
signal ps :  std_logic_vector(4 downto 0);
   -- timing of ps: 14.520000ns
signal level0 :  std_logic_vector(26 downto 0);
   -- timing of level0: 9.010000ns
signal level1 :  std_logic_vector(27 downto 0);
   -- timing of level1: 14.520000ns
signal level2 :  std_logic_vector(29 downto 0);
   -- timing of level2: 15.531538ns
signal level3 :  std_logic_vector(33 downto 0);
   -- timing of level3: 15.531538ns
signal level4 :  std_logic_vector(41 downto 0);
   -- timing of level4: 16.727692ns
signal level5 :  std_logic_vector(57 downto 0);
   -- timing of level5: 16.727692ns
begin
   ps<= S;
   level0<= X;
   level1<= level0 & (0 downto 0 => '0') when ps(0)= '1' else     (0 downto 0 => '0') & level0;
   level2<= level1 & (1 downto 0 => '0') when ps(1)= '1' else     (1 downto 0 => '0') & level1;
   level3<= level2 & (3 downto 0 => '0') when ps(2)= '1' else     (3 downto 0 => '0') & level2;
   level4<= level3 & (7 downto 0 => '0') when ps(3)= '1' else     (7 downto 0 => '0') & level3;
   level5<= level4 & (15 downto 0 => '0') when ps(4)= '1' else     (15 downto 0 => '0') & level4;
   R <= level5(52 downto 0);
end architecture;

--------------------------------------------------------------------------------
--                           IntAdder_31_comb_uid13
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: 16.727692nsY: 0.000000nsCin: 18.317692ns
--  approx. output signal timings: R: 19.617692ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_31_comb_uid13 is
    port (X : in  std_logic_vector(30 downto 0);
          Y : in  std_logic_vector(30 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(30 downto 0)   );
end entity;

architecture arch of IntAdder_31_comb_uid13 is
signal Rtmp :  std_logic_vector(30 downto 0);
   -- timing of Rtmp: 19.617692ns
begin
   Rtmp <= X + Y + Cin;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                          IEEEFPAdd_8_23_comb_uid2
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Florent de Dinechin, Valentin Huguet (2016)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: 0.000000nsY: 0.000000ns
--  approx. output signal timings: R: 21.277692ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IEEEFPAdd_8_23_comb_uid2 is
    port (X : in  std_logic_vector(31 downto 0);
          Y : in  std_logic_vector(31 downto 0);
          R : out  std_logic_vector(31 downto 0)   );
end entity;

architecture arch of IEEEFPAdd_8_23_comb_uid2 is
   component RightShifterSticky26_by_max_25_comb_uid4 is
      port ( X : in  std_logic_vector(25 downto 0);
             S : in  std_logic_vector(4 downto 0);
             R : out  std_logic_vector(25 downto 0);
             Sticky : out  std_logic   );
   end component;

   component IntAdder_27_comb_uid6 is
      port ( X : in  std_logic_vector(26 downto 0);
             Y : in  std_logic_vector(26 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(26 downto 0)   );
   end component;

   component LZC_26_comb_uid8 is
      port ( I : in  std_logic_vector(25 downto 0);
             O : out  std_logic_vector(4 downto 0)   );
   end component;

   component LeftShifter27_by_max_26_comb_uid10 is
      port ( X : in  std_logic_vector(26 downto 0);
             S : in  std_logic_vector(4 downto 0);
             R : out  std_logic_vector(52 downto 0)   );
   end component;

   component IntAdder_31_comb_uid13 is
      port ( X : in  std_logic_vector(30 downto 0);
             Y : in  std_logic_vector(30 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(30 downto 0)   );
   end component;

signal expFracX :  std_logic_vector(30 downto 0);
   -- timing of expFracX: 0.000000ns
signal expFracY :  std_logic_vector(30 downto 0);
   -- timing of expFracY: 0.000000ns
signal expXmExpY :  std_logic_vector(8 downto 0);
   -- timing of expXmExpY: 1.080000ns
signal expYmExpX :  std_logic_vector(8 downto 0);
   -- timing of expYmExpX: 1.080000ns
signal swap :  std_logic;
   -- timing of swap: 1.150000ns
signal newX :  std_logic_vector(31 downto 0);
   -- timing of newX: 1.700000ns
signal newY :  std_logic_vector(31 downto 0);
   -- timing of newY: 1.700000ns
signal expDiff :  std_logic_vector(8 downto 0);
   -- timing of expDiff: 1.700000ns
signal expNewX :  std_logic_vector(7 downto 0);
   -- timing of expNewX: 1.700000ns
signal expNewY :  std_logic_vector(7 downto 0);
   -- timing of expNewY: 1.700000ns
signal signNewX :  std_logic;
   -- timing of signNewX: 1.700000ns
signal signNewY :  std_logic;
   -- timing of signNewY: 1.700000ns
signal EffSub :  std_logic;
   -- timing of EffSub: 2.250000ns
signal xExpFieldZero :  std_logic;
   -- timing of xExpFieldZero: 2.250000ns
signal yExpFieldZero :  std_logic;
   -- timing of yExpFieldZero: 2.250000ns
signal xExpFieldAllOnes :  std_logic;
   -- timing of xExpFieldAllOnes: 2.250000ns
signal yExpFieldAllOnes :  std_logic;
   -- timing of yExpFieldAllOnes: 2.250000ns
signal xSigFieldZero :  std_logic;
   -- timing of xSigFieldZero: 2.250000ns
signal ySigFieldZero :  std_logic;
   -- timing of ySigFieldZero: 2.250000ns
signal xIsNaN :  std_logic;
   -- timing of xIsNaN: 2.800000ns
signal yIsNaN :  std_logic;
   -- timing of yIsNaN: 2.800000ns
signal xIsInfinity :  std_logic;
   -- timing of xIsInfinity: 2.800000ns
signal yIsInfinity :  std_logic;
   -- timing of yIsInfinity: 2.800000ns
signal xIsZero :  std_logic;
   -- timing of xIsZero: 2.800000ns
signal yIsZero :  std_logic;
   -- timing of yIsZero: 2.800000ns
signal bothSubNormals :  std_logic;
   -- timing of bothSubNormals: 2.800000ns
signal resultIsNaN :  std_logic;
   -- timing of resultIsNaN: 3.350000ns
signal significandNewX :  std_logic_vector(23 downto 0);
   -- timing of significandNewX: 2.250000ns
signal significandNewY :  std_logic_vector(23 downto 0);
   -- timing of significandNewY: 2.250000ns
signal allShiftedOut :  std_logic;
   -- timing of allShiftedOut: 2.770000ns
signal rightShiftValue :  std_logic_vector(4 downto 0);
   -- timing of rightShiftValue: 3.320000ns
signal shiftCorrection :  std_logic;
   -- timing of shiftCorrection: 2.800000ns
signal finalRightShiftValue :  std_logic_vector(4 downto 0);
   -- timing of finalRightShiftValue: 4.360000ns
signal significandY00 :  std_logic_vector(25 downto 0);
   -- timing of significandY00: 2.250000ns
signal shiftedSignificandY :  std_logic_vector(25 downto 0);
   -- timing of shiftedSignificandY: 5.460000ns
signal stickyLow :  std_logic;
   -- timing of stickyLow: 7.750000ns
signal summandY :  std_logic_vector(26 downto 0);
   -- timing of summandY: 6.010000ns
signal summandX :  std_logic_vector(26 downto 0);
   -- timing of summandX: 2.250000ns
signal carryIn :  std_logic;
   -- timing of carryIn: 7.750000ns
signal significandZ :  std_logic_vector(26 downto 0);
   -- timing of significandZ: 9.010000ns
signal z1 :  std_logic;
   -- timing of z1: 9.010000ns
signal z0 :  std_logic;
   -- timing of z0: 9.010000ns
signal lzcZInput :  std_logic_vector(25 downto 0);
   -- timing of lzcZInput: 9.010000ns
signal lzc :  std_logic_vector(4 downto 0);
   -- timing of lzc: 12.930000ns
signal leftShiftVal :  std_logic_vector(4 downto 0);
   -- timing of leftShiftVal: 14.520000ns
signal normalizedSignificand :  std_logic_vector(52 downto 0);
   -- timing of normalizedSignificand: 16.727692ns
signal significandPreRound :  std_logic_vector(22 downto 0);
   -- timing of significandPreRound: 16.727692ns
signal lsb :  std_logic;
   -- timing of lsb: 16.727692ns
signal roundBit :  std_logic;
   -- timing of roundBit: 16.727692ns
signal stickyBit :  std_logic;
   -- timing of stickyBit: 17.767692ns
signal deltaExp :  std_logic_vector(7 downto 0);
   -- timing of deltaExp: 12.930000ns
signal fullCancellation :  std_logic;
   -- timing of fullCancellation: 13.930000ns
signal expPreRound :  std_logic_vector(7 downto 0);
   -- timing of expPreRound: 14.000000ns
signal expSigPreRound :  std_logic_vector(30 downto 0);
   -- timing of expSigPreRound: 16.727692ns
signal roundUpBit :  std_logic;
   -- timing of roundUpBit: 18.317692ns
signal expSigR :  std_logic_vector(30 downto 0);
   -- timing of expSigR: 19.617692ns
signal resultIsZero :  std_logic;
   -- timing of resultIsZero: 20.727692ns
signal resultIsInf :  std_logic;
   -- timing of resultIsInf: 20.727692ns
signal constInf :  std_logic_vector(30 downto 0);
   -- timing of constInf: 0.000000ns
signal constNaN :  std_logic_vector(30 downto 0);
   -- timing of constNaN: 0.000000ns
signal expSigR2 :  std_logic_vector(30 downto 0);
   -- timing of expSigR2: 21.277692ns
signal signR :  std_logic;
   -- timing of signR: 21.277692ns
signal computedR :  std_logic_vector(31 downto 0);
   -- timing of computedR: 21.277692ns
begin

   -- Exponent difference and swap
   expFracX <= X(30 downto 0);
   expFracY <= Y(30 downto 0);
   expXmExpY <= ('0' & X(30 downto 23)) - ('0'  & Y(30 downto 23)) ;
   expYmExpX <= ('0' & Y(30 downto 23)) - ('0'  & X(30 downto 23)) ;
   swap <= '0' when expFracX >= expFracY else '1';
   newX <= X when swap = '0' else Y;
   newY <= Y when swap = '0' else X;
   expDiff <= expXmExpY when swap = '0' else expYmExpX;
   expNewX <= newX(30 downto 23);
   expNewY <= newY(30 downto 23);
   signNewX <= newX(31);
   signNewY <= newY(31);
   EffSub <= signNewX xor signNewY;
   -- Special case dectection
   xExpFieldZero <= '1' when expNewX="00000000" else '0';
   yExpFieldZero <= '1' when expNewY="00000000" else '0';
   xExpFieldAllOnes <= '1' when expNewX="11111111" else '0';
   yExpFieldAllOnes <= '1' when expNewY="11111111" else '0';
   xSigFieldZero <= '1' when newX(22 downto 0)="00000000000000000000000" else '0';
   ySigFieldZero <= '1' when newY(22 downto 0)="00000000000000000000000" else '0';
   xIsNaN <= xExpFieldAllOnes and not xSigFieldZero;
   yIsNaN <= yExpFieldAllOnes and not ySigFieldZero;
   xIsInfinity <= xExpFieldAllOnes and xSigFieldZero;
   yIsInfinity <= yExpFieldAllOnes and ySigFieldZero;
   xIsZero <= xExpFieldZero and xSigFieldZero;
   yIsZero <= yExpFieldZero and ySigFieldZero;
   bothSubNormals <=  xExpFieldZero and yExpFieldZero;
   resultIsNaN <=  xIsNaN or yIsNaN  or  (xIsInfinity and yIsInfinity and EffSub);
   significandNewX <= not(xExpFieldZero) & newX(22 downto 0);
   significandNewY <= not(yExpFieldZero) & newY(22 downto 0);

   -- Significand alignment
   allShiftedOut <= '1' when (expDiff >= 26) else '0';
   rightShiftValue <= expDiff(4 downto 0) when allShiftedOut='0' else CONV_STD_LOGIC_VECTOR(26,5) ;
   shiftCorrection <= '1' when (yExpFieldZero='1' and xExpFieldZero='0') else '0'; -- only other cases are: both normal or both subnormal
   finalRightShiftValue <= rightShiftValue - ("0000" & shiftCorrection);
   significandY00 <= significandNewY & "00";
   RightShifterComponent: RightShifterSticky26_by_max_25_comb_uid4
      port map ( S => finalRightShiftValue,
                 X => significandY00,
                 R => shiftedSignificandY,
                 Sticky => stickyLow);
   summandY <= ('0' & shiftedSignificandY) xor (26 downto 0 => EffSub);


   -- Significand addition
   summandX <= '0' & significandNewX & '0' & '0';
   carryIn <= EffSub and not stickyLow;
   fracAdder: IntAdder_27_comb_uid6
      port map ( Cin => carryIn,
                 X => summandX,
                 Y => summandY,
                 R => significandZ);

   -- Cancellation detection, renormalization (see explanations in IEEEFPAdd.cpp) 
   z1 <=  significandZ(26); -- bit of weight 1
   z0 <=  significandZ(25); -- bit of weight 0
   lzcZInput <= significandZ(26 downto 1);
   IEEEFPAdd_8_23_comb_uid2LeadingZeroCounter: LZC_26_comb_uid8
      port map ( I => lzcZInput,
                 O => lzc);
   leftShiftVal <= 
      lzc when ((z1='1') or (z1='0' and z0='1' and xExpFieldZero='1') or (z1='0' and z0='0' and xExpFieldZero='0' and lzc<=expNewX)  or (xExpFieldZero='0' and lzc>=26) ) 
      else (expNewX(4 downto 0)) when (xExpFieldZero='0' and (lzc < 26) and (("000"&lzc)>=expNewX)) 
       else "0000"&'1';
   LeftShifterComponent: LeftShifter27_by_max_26_comb_uid10
      port map ( S => leftShiftVal,
                 X => significandZ,
                 R => normalizedSignificand);
   significandPreRound <= normalizedSignificand(25 downto 3); -- remove the implicit zero/one
   lsb <= normalizedSignificand(3);
   roundBit <= normalizedSignificand(2);
   stickyBit <= stickyLow or  normalizedSignificand(1)or  normalizedSignificand(0);
   deltaExp <=    -- value to subtract to exponent for normalization
      "00000000" when ( (z1='0' and z0='1' and xExpFieldZero='0')
          or  (z1='0' and z0='0' and xExpFieldZero='1') )
      else "11111111" when ( (z1='1')  or  (z1='0' and z0='1' and xExpFieldZero='1'))
      else ("000" & lzc)-'1' when (z1='0' and z0='0' and xExpFieldZero='0' and lzc<=expNewX and lzc<26)      else expNewX;
   fullCancellation <= '1' when (lzc>=26) else '0';
   expPreRound <= expNewX - deltaExp; -- we may have a first overflow here
   expSigPreRound <= expPreRound & significandPreRound; 
   -- Final rounding, with the mantissa overflowing in the exponent  
   roundUpBit <= '1' when roundBit='1' and (stickyBit='1' or (stickyBit='0' and lsb='1')) else '0';
   roundingAdder: IntAdder_31_comb_uid13
      port map ( Cin => roundUpBit,
                 X => expSigPreRound,
                 Y => "0000000000000000000000000000000",
                 R => expSigR);
   -- Final packing
   resultIsZero <= '1' when (fullCancellation='1' and expSigR(30 downto 23)="00000000") else '0';
   resultIsInf <= '1' when resultIsNaN='0' and (((xIsInfinity='1' and yIsInfinity='1'  and EffSub='0')  or (xIsInfinity='0' and yIsInfinity='1')  or (xIsInfinity='1' and yIsInfinity='0')  or  (expSigR(30 downto 23)="11111111"))) else '0';
   constInf <= "11111111" & "00000000000000000000000";
   constNaN <= "1111111111111111111111111111111";
   expSigR2 <= constInf when resultIsInf='1' else constNaN when resultIsNaN='1' else expSigR;
   signR <= '0' when ((resultIsNaN='1'  or (resultIsZero='1' and xIsInfinity='0' and yIsInfinity='0')) and (xIsZero='0' or yIsZero='0' or (signNewX /= signNewY)) )  else signNewX;
   computedR <= signR & expSigR2;
   R <= computedR;
end architecture;

--------------------------------------------------------------------------------
--                      IntMultiplier_24x24_48_comb_uid5
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Martin Kumm, Florent de Dinechin, Andreas Böttcher, Kinga Illyes, Bogdan Popa, Bogdan Pasca, 2012-
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: 0.000000nsY: 0.000000ns
--  approx. output signal timings: R: 0.000000ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library std;
use std.textio.all;
library work;

entity IntMultiplier_24x24_48_comb_uid5 is
    port (X : in  std_logic_vector(23 downto 0);
          Y : in  std_logic_vector(23 downto 0);
          R : out  std_logic_vector(47 downto 0)   );
end entity;

architecture arch of IntMultiplier_24x24_48_comb_uid5 is
signal XX_m6 :  std_logic_vector(23 downto 0);
   -- timing of XX_m6: 0.000000ns
signal YY_m6 :  std_logic_vector(23 downto 0);
   -- timing of YY_m6: 0.000000ns
signal XX :  unsigned(-1+24 downto 0);
   -- timing of XX: 0.000000ns
signal YY :  unsigned(-1+24 downto 0);
   -- timing of YY: 0.000000ns
signal RR :  unsigned(-1+48 downto 0);
   -- timing of RR: 0.000000ns
begin
   XX_m6 <= X ;
   YY_m6 <= Y ;
   XX <= unsigned(X);
   YY <= unsigned(Y);
   RR <= XX*YY;
   R <= std_logic_vector(RR(47 downto 0));
end architecture;

--------------------------------------------------------------------------------
--                           IntAdder_33_comb_uid9
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin (2008-2016)
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X Y Cin
-- Output signals: R
--  approx. input signal timings: X: 2.180000nsY: 0.000000nsCin: 1.700000ns
--  approx. output signal timings: R: 3.500000ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity IntAdder_33_comb_uid9 is
    port (X : in  std_logic_vector(32 downto 0);
          Y : in  std_logic_vector(32 downto 0);
          Cin : in  std_logic;
          R : out  std_logic_vector(32 downto 0)   );
end entity;

architecture arch of IntAdder_33_comb_uid9 is
signal Rtmp :  std_logic_vector(32 downto 0);
   -- timing of Rtmp: 3.500000ns
begin
   Rtmp <= X + Y + Cin;
   R <= Rtmp;
end architecture;

--------------------------------------------------------------------------------
--                         FPMult_8_23_uid2_comb_uid3
-- VHDL generated for DummyFPGA @ 0MHz
-- This operator is part of the Infinite Virtual Library FloPoCoLib
-- All rights reserved 
-- Authors: Bogdan Pasca, Florent de Dinechin 2008-2021
--------------------------------------------------------------------------------
-- combinatorial
-- Clock period (ns): inf
-- Target frequency (MHz): 0
-- Input signals: X Y
-- Output signals: R
--  approx. input signal timings: X: 0.000000nsY: 0.000000ns
--  approx. output signal timings: R: 3.500000ns

library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;
library std;
use std.textio.all;
library work;

entity FPMult_8_23_uid2_comb_uid3 is
    port (X : in  std_logic_vector(8+23+2 downto 0);
          Y : in  std_logic_vector(8+23+2 downto 0);
          R : out  std_logic_vector(8+23+2 downto 0)   );
end entity;

architecture arch of FPMult_8_23_uid2_comb_uid3 is
   component IntMultiplier_24x24_48_comb_uid5 is
      port ( X : in  std_logic_vector(23 downto 0);
             Y : in  std_logic_vector(23 downto 0);
             R : out  std_logic_vector(47 downto 0)   );
   end component;

   component IntAdder_33_comb_uid9 is
      port ( X : in  std_logic_vector(32 downto 0);
             Y : in  std_logic_vector(32 downto 0);
             Cin : in  std_logic;
             R : out  std_logic_vector(32 downto 0)   );
   end component;

signal sign :  std_logic;
   -- timing of sign: 0.050000ns
signal expX :  std_logic_vector(7 downto 0);
   -- timing of expX: 0.000000ns
signal expY :  std_logic_vector(7 downto 0);
   -- timing of expY: 0.000000ns
signal expSumPreSub :  std_logic_vector(9 downto 0);
   -- timing of expSumPreSub: 1.090000ns
signal bias :  std_logic_vector(9 downto 0);
   -- timing of bias: 0.000000ns
signal expSum :  std_logic_vector(9 downto 0);
   -- timing of expSum: 2.180000ns
signal sigX :  std_logic_vector(23 downto 0);
   -- timing of sigX: 0.000000ns
signal sigY :  std_logic_vector(23 downto 0);
   -- timing of sigY: 0.000000ns
signal sigProd :  std_logic_vector(47 downto 0);
   -- timing of sigProd: 0.000000ns
signal excSel :  std_logic_vector(3 downto 0);
   -- timing of excSel: 0.000000ns
signal exc :  std_logic_vector(1 downto 0);
   -- timing of exc: 0.050000ns
signal norm :  std_logic;
   -- timing of norm: 0.000000ns
signal expPostNorm :  std_logic_vector(9 downto 0);
   -- timing of expPostNorm: 2.180000ns
signal sigProdExt :  std_logic_vector(47 downto 0);
   -- timing of sigProdExt: 0.550000ns
signal expSig :  std_logic_vector(32 downto 0);
   -- timing of expSig: 2.180000ns
signal sticky :  std_logic;
   -- timing of sticky: 0.550000ns
signal guard :  std_logic;
   -- timing of guard: 1.150000ns
signal round :  std_logic;
   -- timing of round: 1.700000ns
signal expSigPostRound :  std_logic_vector(32 downto 0);
   -- timing of expSigPostRound: 3.500000ns
signal excPostNorm :  std_logic_vector(1 downto 0);
   -- timing of excPostNorm: 3.500000ns
signal finalExc :  std_logic_vector(1 downto 0);
   -- timing of finalExc: 3.500000ns
begin
   sign <= X(31) xor Y(31);
   expX <= X(30 downto 23);
   expY <= Y(30 downto 23);
   expSumPreSub <= ("00" & expX) + ("00" & expY);
   bias <= CONV_STD_LOGIC_VECTOR(127,10);
   expSum <= expSumPreSub - bias;
   sigX <= "1" & X(22 downto 0);
   sigY <= "1" & Y(22 downto 0);
   SignificandMultiplication: IntMultiplier_24x24_48_comb_uid5
      port map ( X => sigX,
                 Y => sigY,
                 R => sigProd);
   excSel <= X(33 downto 32) & Y(33 downto 32);
   with excSel  select  
   exc <= "00" when  "0000" | "0001" | "0100", 
          "01" when "0101",
          "10" when "0110" | "1001" | "1010" ,
          "11" when others;
   norm <= sigProd(47);
   -- exponent update
   expPostNorm <= expSum + ("000000000" & norm);
   -- significand normalization shift
   sigProdExt <= sigProd(46 downto 0) & "0" when norm='1' else
                         sigProd(45 downto 0) & "00";
   expSig <= expPostNorm & sigProdExt(47 downto 25);
   sticky <= sigProdExt(24);
   guard <= '0' when sigProdExt(23 downto 0)="000000000000000000000000" else '1';
   round <= sticky and ( (guard and not(sigProdExt(25))) or (sigProdExt(25) ))  ;
   RoundingAdder: IntAdder_33_comb_uid9
      port map ( Cin => round,
                 X => expSig,
                 Y => "000000000000000000000000000000000",
                 R => expSigPostRound);
   with expSigPostRound(32 downto 31)  select 
   excPostNorm <=  "01"  when  "00",
                               "10"             when "01", 
                               "00"             when "11"|"10",
                               "11"             when others;
   with exc  select  
   finalExc <= exc when  "11"|"10"|"00",
                       excPostNorm when others; 
   R <= finalExc & sign & expSigPostRound(30 downto 0);
end architecture;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FP_Register_File is
    Port (
        clk : in  STD_LOGIC;
        we3 : in  STD_LOGIC;
        A1  : in  STD_LOGIC_VECTOR (4 downto 0);
        A2  : in  STD_LOGIC_VECTOR (4 downto 0);
        A3  : in  STD_LOGIC_VECTOR (4 downto 0);
        RD1 : out STD_LOGIC_VECTOR (31 downto 0);
        RD2 : out STD_LOGIC_VECTOR (31 downto 0);
        WD3 : in  STD_LOGIC_VECTOR (31 downto 0);
        -- debug probe (mesma ideia que no seu RF inteiro)
        probe_index : in  STD_LOGIC_VECTOR(4 downto 0);
        probe_data  : out STD_LOGIC_VECTOR(31 downto 0)
    );
end FP_Register_File;

architecture Behavioral of FP_Register_File is

    type ram_type is array(0 to 31) of std_logic_vector(31 downto 0);
    -- inicializa todos os registradores com zero
    signal ram : ram_type :=
    (
        0 => x"451C4000",  -- +2500.0
        1 => x"C4DAC000",  -- -1750.0
        others => (others => '0')
    );

begin

    -- escrita síncrona (FP regs podem escrever em r0 também)
    process(clk)
    begin
        if rising_edge(clk) then
            if we3 = '1' then
                ram(to_integer(unsigned(A3))) <= WD3;
            end if;
        end if;
    end process;

    -- leitura combinacional
    process(A1, A2, ram)
    begin
        RD1 <= ram(to_integer(unsigned(A1)));
        RD2 <= ram(to_integer(unsigned(A2)));
    end process;

    -- probe combinacional para depuração
    process(probe_index, ram)
        variable idx : integer;
    begin
        idx := to_integer(unsigned(probe_index));
        if idx >= 0 and idx <= 31 then
            probe_data <= ram(idx);
        else
            probe_data <= (others => 'X');
        end if;
    end process;

end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity FP_Control_Unit is
  generic (
    FMT_SINGLE  : std_logic_vector(4 downto 0) := "10000";
    FUNCT_ADD_S : std_logic_vector(5 downto 0) := "000000";
    FUNCT_MUL_S : std_logic_vector(5 downto 0) := "000010"
  );
  port (
    clk        : in  std_logic;
    reset      : in  std_logic;

    -- decode inputs
    is_fp      : in  std_logic;
    fmt        : in  std_logic_vector(4 downto 0);
    funct      : in  std_logic_vector(5 downto 0);
    fs         : in  std_logic_vector(4 downto 0);
    ft         : in  std_logic_vector(4 downto 0);
    fd         : in  std_logic_vector(4 downto 0);

    -- debug probe into internal FP RF
    probe_index : in  std_logic_vector(4 downto 0);
    probe_data  : out std_logic_vector(31 downto 0);

    -- control outputs
    stall      : out std_logic;
    busy       : out std_logic
  );
end FP_Control_Unit;

architecture Behavioral of FP_Control_Unit is

  -- === internal signals between control and RF / FloPoCo ===
  signal fp_rd1   : std_logic_vector(31 downto 0);
  signal fp_rd2   : std_logic_vector(31 downto 0);
  signal fp_ra1   : std_logic_vector(4 downto 0);
  signal fp_ra2   : std_logic_vector(4 downto 0);
  signal fp_wa    : std_logic_vector(4 downto 0);
  signal fp_we    : std_logic := '0';
  signal fp_wd    : std_logic_vector(31 downto 0);

  -- FloPoCo signals
  signal add_R    : std_logic_vector(31 downto 0);  -- adder 32-bit result
  signal mul_X    : std_logic_vector(33 downto 0);
  signal mul_Y    : std_logic_vector(33 downto 0);
  signal mul_R    : std_logic_vector(33 downto 0);

  -- control FSM
  type state_t is (IDLE, SETUP, WRITEBACK, WAITING);
  signal state       : state_t := IDLE;
  signal do_add      : std_logic := '0';
  signal do_mul      : std_logic := '0';
  signal is_on_wb     : std_logic := '0';
  
  -- helper: hex string for 32-bit vector
  function to_hex32(v : std_logic_vector(31 downto 0)) return string is
    variable res : string(1 to 8);
    variable nib : std_logic_vector(3 downto 0);
    variable val : integer;
    variable idx_bit : integer;
  begin
    for i in 0 to 7 loop
      idx_bit := 31 - i*4;
      nib(3) := v(idx_bit);
      nib(2) := v(idx_bit-1);
      nib(1) := v(idx_bit-2);
      nib(0) := v(idx_bit-3);
      val := to_integer(unsigned(nib));
      if val < 10 then
        res(i+1) := character'VAL(character'POS('0') + val);
      else
        res(i+1) := character'VAL(character'POS('A') + (val - 10));
      end if;
    end loop;
    return res;
  end function;

  -- Component declarations (FP RF and FloPoCo modules)
  component FP_Register_File
    Port (
      clk : in  STD_LOGIC;
      we3 : in  STD_LOGIC;
      A1  : in  STD_LOGIC_VECTOR (4 downto 0);
      A2  : in  STD_LOGIC_VECTOR (4 downto 0);
      A3  : in  STD_LOGIC_VECTOR (4 downto 0);
      RD1 : out STD_LOGIC_VECTOR (31 downto 0);
      RD2 : out STD_LOGIC_VECTOR (31 downto 0);
      WD3 : in  STD_LOGIC_VECTOR (31 downto 0);
      probe_index : in  STD_LOGIC_VECTOR(4 downto 0);
      probe_data  : out STD_LOGIC_VECTOR(31 downto 0)
    );
  end component;

  -- adder: your version with 32-bit X/Y/R
  component IEEEFPAdd_8_23_comb_uid2
    port (
      X : in  std_logic_vector(31 downto 0);
      Y : in  std_logic_vector(31 downto 0);
      R : out std_logic_vector(31 downto 0)
    );
  end component;

  -- multiplier: FloPoCo 34-bit
  component FPMult_8_23_uid2_comb_uid3
    port (
      X : in  std_logic_vector(33 downto 0);
      Y : in  std_logic_vector(33 downto 0);
      R : out std_logic_vector(33 downto 0)
    );
  end component;

begin

  ---------------------------------------------------------------------
  -- Instantiate internal FP Register File
  ---------------------------------------------------------------------
  rf_inst : FP_Register_File
    port map (
      clk => clk,
      we3 => fp_we,
      A1  => fp_ra1,
      A2  => fp_ra2,
      A3  => fp_wa,
      RD1 => fp_rd1,
      RD2 => fp_rd2,
      WD3 => fp_wd,
      probe_index => probe_index,
      probe_data  => probe_data
    );

  ---------------------------------------------------------------------
  -- Instantiate FloPoCo combinational FP add and FP mul
  ---------------------------------------------------------------------
  fpadd_inst: IEEEFPAdd_8_23_comb_uid2
    port map (
      X => fp_rd1,   -- 32 bits
      Y => fp_rd2,   -- 32 bits
      R => add_R     -- 32-bit result
    );

  fpmul_inst: FPMult_8_23_uid2_comb_uid3
    port map (
      X => mul_X,
      Y => mul_Y,
      R => mul_R
    );

  ---------------------------------------------------------------------
  -- Connect FloPoCo inputs/outputs
  -- multiplier expects 34-bit inputs padded with "00"
  -- adder returns 32-bit: pad to 34 bits as before
  ---------------------------------------------------------------------
  mul_X <= "00" & fp_rd1;
  mul_Y <= "00" & fp_rd2;

  ---------------------------------------------------------------------
  -- Drive FP read addresses directly from instruction fields
  ---------------------------------------------------------------------
  fp_ra1 <= fs;
  fp_ra2 <= ft;
  fp_wa  <= fd;
  stall <= is_fp and not (is_on_wb);
  ---------------------------------------------------------------------
  -- Control FSM (same behavior as sua versão original, but using internal signals)
  ---------------------------------------------------------------------
  process(clk, reset)
  begin
    if reset = '1' then
      state <= IDLE;
      do_add <= '0';
      do_mul <= '0';
      fp_we <= '0';
      fp_wd <= (others => '0');
    elsif rising_edge(clk) then
      case state is
        when IDLE =>
          do_add <= '0';
          do_mul <= '0';
          -- permanece em IDLE até haver uma instrução FP válida
          if is_fp = '1' then
            if fmt = FMT_SINGLE and funct = FUNCT_ADD_S then
              do_add <= '1';
              do_mul <= '0';
              state <= SETUP;
            elsif fmt = FMT_SINGLE and funct = FUNCT_MUL_S then
              do_add <= '0';
              do_mul <= '1';
              state <= SETUP;
            else
              -- invalida: permanece em IDLE
              state <= IDLE;
            end if;
          else
            state <= IDLE;
          end if;

        when SETUP =>
          -- prepara a operação: trava a pipeline/estágio FP
          fp_we <= '0'; -- garantir 0 até querermos escrever
          is_on_wb <= '0';
          state <= WRITEBACK;

        when WRITEBACK =>
          -- neste ciclo a RF já verá we3 = '1' (fp_we_reg) na borda anterior,
          -- portanto apenas colocamos o dado a ser escrito (fp_wd) e fazemos limpeza
		  fp_we <= '1';
          if do_add = '1' then
            fp_wd <= add_R(31 downto 0);
          elsif do_mul = '1' then
            fp_wd <= mul_R(31 downto 0);
          else
            fp_wd <= (others => '0');
          end if;
          -- reset de flags e volta ao IDLE
          do_add <= '0';
          do_mul <= '0';
          is_on_wb <= '1';
          state <= WAITING;
        when WAITING =>
          is_on_wb <= '0';
          fp_we <= '0';
          state <= IDLE;      
        when others =>
          -- segurança: volta ao IDLE
          state <= IDLE;

      end case;
    end if;
  end process;

end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity zero_extend is
    Port ( inic : in  STD_LOGIC;
           extend : out  STD_LOGIC_VECTOR (31 downto 0));
end zero_extend;

architecture Behavioral of zero_extend is

begin
	extend<=x"0000000"&"000"&inic;

end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity Mux_2to1_32b is
    Port ( ctrl : in  STD_LOGIC;
           A : in  STD_LOGIC_VECTOR (31 downto 0);
           B : in  STD_LOGIC_VECTOR (31 downto 0);
           O : out  STD_LOGIC_VECTOR (31 downto 0));
end Mux_2to1_32b;

architecture Behavioral of Mux_2to1_32b is

begin
	o <= 	a 	when ctrl='0' else b;
			
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity alu_sum_res is
    Port (
        ctrl : in  STD_LOGIC;
        a    : in  STD_LOGIC_VECTOR (31 downto 0);
        b    : in  STD_LOGIC_VECTOR (31 downto 0);
        sol  : out STD_LOGIC_VECTOR (31 downto 0);
        cout : out STD_LOGIC
    );
end alu_sum_res;

architecture Behavioral of alu_sum_res is
  signal result    : unsigned(32 downto 0);
  signal carry_in  : unsigned(32 downto 0);
  signal carry_slv : std_logic_vector(32 downto 0);
begin

  -- crie explicitamente um std_logic_vector de 33 bits com ctrl no LSB
  carry_slv <= (32 downto 1 => '0') & ctrl;

  -- converta para unsigned (agora é legal)
  carry_in <= unsigned(carry_slv);

  -- estenda a e b para 33 bits como std_logic_vector e converta para unsigned
  result <= unsigned("0" & a) + unsigned("0" & b) + carry_in;

  -- converta resultado para saídas
  sol  <= std_logic_vector(result(31 downto 0));
  cout <= result(32);

end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity mux_4_32b is
    Port ( ctrl : in  STD_LOGIC_VECTOR (1 downto 0);
           a : in  STD_LOGIC_VECTOR (31 downto 0);
           b : in  STD_LOGIC_VECTOR (31 downto 0);
           c : in  STD_LOGIC_VECTOR (31 downto 0);
           d : in  STD_LOGIC_VECTOR (31 downto 0);
           sal : out  STD_LOGIC_VECTOR (31 downto 0));
end mux_4_32b;

architecture Behavioral of mux_4_32b is

begin
	sal<= a when (ctrl="00") else
			b when (ctrl="01") else
			c when (ctrl="10") else
			d;

end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;



entity ALU is
    Port ( a : in  STD_LOGIC_VECTOR (31 downto 0);
           b : in  STD_LOGIC_VECTOR (31 downto 0);
           func : in  STD_LOGIC_VECTOR (2 downto 0);
		   zero: out std_logic ;
           rslt : out  STD_LOGIC_VECTOR (31 downto 0));
end ALU;

architecture Behavioral of ALU is
	COMPONENT zero_extend
	PORT(
		inic : IN std_logic;          
		extend : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT Mux_2to1_32b
	PORT(
		ctrl : IN std_logic;
		A : IN std_logic_vector(31 downto 0);
		B : IN std_logic_vector(31 downto 0);          
		O : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT alu_sum_res
	PORT(
		ctrl : IN std_logic;
		a : IN std_logic_vector(31 downto 0);
		b : IN std_logic_vector(31 downto 0);          
		sol : OUT std_logic_vector(31 downto 0);
		cout : OUT std_logic
		);
	END COMPONENT;
	
	COMPONENT mux_4_32b
	PORT(
		ctrl : IN std_logic_vector(1 downto 0);
		a : IN std_logic_vector(31 downto 0);
		b : IN std_logic_vector(31 downto 0);
		c : IN std_logic_vector(31 downto 0);
		d : IN std_logic_vector(31 downto 0);          
		sal : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	
	signal	rslt_and,rslt_or,rslt_and_compl,rslt_mux_alu,
					rslt_or_compl,rslt_slt,bb,b_compl,rslt_sum_res : std_logic_vector (31 downto 0):=x"00000000";
	
begin
	
	b_compl<=not(b);
	
	mux_b_b_compl: Mux_2to1_32b PORT MAP(
		ctrl => func(2),
		A => b,
		B => b_compl,
		O => bb
	);
	
	Inst_alu_sum_res: alu_sum_res PORT MAP(
		ctrl => func(2),
		a => a,
		b => bb,
		sol => rslt_sum_res
	);
	
	rslt_and<=a and bb;
	rslt_or<=a or bb;
	
	Inst_zero_extend: zero_extend PORT MAP(
		inic => rslt_sum_res(31),
		extend => rslt_slt
	);
	
	mux_rslt_alu: mux_4_32b PORT MAP(
		ctrl => func(1 downto 0),
		a => rslt_and,
		b => rslt_or,
		c => rslt_sum_res,
		d => rslt_slt,
		sal => rslt_mux_alu
	);
	
	
	zero<=	'1' when (rslt_mux_alu=x"00000000") else 
				'0';
	rslt<=rslt_mux_alu;
				
end Behavioral;

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity SignExtend is
    Port ( din : in  STD_LOGIC_VECTOR (15 downto 0);
           dout : out  STD_LOGIC_VECTOR (31 downto 0));
end SignExtend;

architecture Behavioral of SignExtend is
	--signal a : std_logic;
begin
	dout<= x"0000"&din when (din(15)='0') else  x"ffff"&din;
		
end Behavioral;


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)


entity corrimiento is
    Port ( din : in  STD_LOGIC_VECTOR (31 downto 0);
           dout : out  STD_LOGIC_VECTOR (31 downto 0));
end corrimiento;

architecture Behavioral of corrimiento is

begin
	dout<=din(29 downto 0)&"00"; --es una multiplicacion por 4
end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;


entity ALU_suma is
    Port ( 
				a : in  STD_LOGIC_VECTOR (31 downto 0);
				b : in  STD_LOGIC_VECTOR (31 downto 0);
				sal : out  STD_LOGIC_VECTOR (31 downto 0));
end ALU_suma;

architecture Behavioral of ALU_suma is

begin
	
	sal <= std_logic_vector(unsigned(a) + unsigned(b));
	
end Behavioral;

-- Bottazzi, Cristian - 2017 (https://github.com/cristian1604/)
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;


entity Mux_2to1_5bits is
    Port ( ctrl : in  STD_LOGIC;
           a : in  STD_LOGIC_VECTOR (4 downto 0);
           b : in  STD_LOGIC_VECTOR (4 downto 0);
           sal : out  STD_LOGIC_VECTOR (4 downto 0));
end Mux_2to1_5bits;

architecture Behavioral of Mux_2to1_5bits is

begin
	sal <= 	a 	when ctrl='0' else b;

end Behavioral;

----------------------------------------------------------------------------------
-- MIPS_FP
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity MIPS is
    Port ( clk : in  STD_LOGIC;
           reset : in std_logic;
           memwrite : out std_logic;
           readdata : in  STD_LOGIC_VECTOR (31 downto 0);
           address : out  STD_LOGIC_VECTOR (31 downto 0);
           writedata : out  STD_LOGIC_VECTOR (31 downto 0);
           -- porta de debug: expõe probe do Register_File
           rf_probe_index : in  std_logic_vector(4 downto 0);
           rf_probe_data  : out std_logic_vector(31 downto 0);
           -- portas de debug: expõe probe do Register_File de PONTO FLUTUANTE (FP)
           fp_probe_index : in  std_logic_vector(4 downto 0);
           fp_probe_data  : out std_logic_vector(31 downto 0)
         );
end MIPS;

architecture Behavioral of MIPS is

	COMPONENT PC
	PORT(
		clk : IN std_logic;
        stall: IN std_logic;
		reset : IN std_logic;
		din : IN std_logic_vector(31 downto 0);          
		dout : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT Instruction_Memory
	PORT(
		dir : IN std_logic_vector(31 downto 0);          
		instr : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT ControlUnit
	PORT(
		OpCode : IN std_logic_vector(5 downto 0);
		Funct : IN std_logic_vector(5 downto 0);          
		MemtoReg : OUT std_logic;
		MemWrite : OUT std_logic;
		Branch : OUT std_logic;
		AluSrc : OUT std_logic;
		RegDst : OUT std_logic;
		RegWrite : OUT std_logic;
		jump : OUT std_logic;
		AluCtrl : OUT std_logic_vector(2 downto 0)
		);
	END COMPONENT;
	
	COMPONENT Register_File
	PORT(
		clk : IN std_logic;
		we3 : IN std_logic;
		A1 : IN std_logic_vector(4 downto 0);
		A2 : IN std_logic_vector(4 downto 0);
		A3 : IN std_logic_vector(4 downto 0);
		WD3 : IN std_logic_vector(31 downto 0);          
		RD1 : OUT std_logic_vector(31 downto 0);
		RD2 : OUT std_logic_vector(31 downto 0);
        -- portas de probe adicionadas (devem corresponder à entity real)
        probe_index : IN std_logic_vector(4 downto 0);
        probe_data  : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
    
    COMPONENT FP_Control_Unit
    PORT(
    	clk        : in  std_logic;
        reset      : in  std_logic;
        is_fp      : in  std_logic;
        fmt        : in  std_logic_vector(4 downto 0);
        funct      : in  std_logic_vector(5 downto 0);
        fs         : in  std_logic_vector(4 downto 0);
        ft         : in  std_logic_vector(4 downto 0);
        fd         : in  std_logic_vector(4 downto 0);
        probe_index : in  std_logic_vector(4 downto 0);
        probe_data  : out std_logic_vector(31 downto 0);
        stall      : out std_logic
    );
    END COMPONENT;
	
	COMPONENT ALU
	PORT(
		a : IN std_logic_vector(31 downto 0);
		b : IN std_logic_vector(31 downto 0);
		func : IN std_logic_vector(2 downto 0);          
		zero : out std_logic;
		rslt : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT SignExtend
	PORT(
		din : IN std_logic_vector(15 downto 0);          
		dout : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT Mux_2to1_32b
	PORT(
		ctrl : IN std_logic;
		A : IN std_logic_vector(31 downto 0);
		B : IN std_logic_vector(31 downto 0);          
		O : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT corrimiento
	PORT(
		din : IN std_logic_vector(31 downto 0);          
		dout : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	COMPONENT ALU_suma
	PORT(
		a : IN std_logic_vector(31 downto 0);
		b : IN std_logic_vector(31 downto 0);          
		sal : OUT std_logic_vector(31 downto 0)
		);
	END COMPONENT;
	
	
	COMPONENT Mux_2to1_5bits
	PORT(
		ctrl : IN std_logic;
		a : IN std_logic_vector(4 downto 0);
		b : IN std_logic_vector(4 downto 0);          
		sal : OUT std_logic_vector(4 downto 0)
		);
	END COMPONENT;
		
	
	signal memtoreg,branch,alusrc,regdst,regwrite,jump,zero : std_logic;
	signal aluctrl : std_logic_vector(2 downto 0);
	
	signal rst_pc : std_logic;
	signal pc_in,pc_out,instr: std_logic_vector(31 downto 0);
	
	alias code_op : std_logic_vector(5 downto 0) is instr(31 downto 26);
	alias funct : std_logic_vector(5 downto 0) is instr(5 downto 0);
	alias rs : std_logic_vector(4 downto 0) is instr(25 downto 21);
	alias rt : std_logic_vector(4 downto 0) is instr(20 downto 16);
	alias rd : std_logic_vector(4 downto 0) is instr(15 downto 11);
	alias shamt : std_logic_vector(4 downto 0) is instr(10 downto 6);
	alias inmd : std_logic_vector(15 downto 0) is instr(15 downto 0);
	alias addr : std_logic_vector(25 downto 0) is instr(25 downto 0);
    alias fmt_sig : std_logic_vector(4 downto 0) is instr(25 downto 21);
    alias ft_sig : std_logic_vector(4 downto 0) is instr(20 downto 16);
    alias fs_sig : std_logic_vector(4 downto 0) is instr(15 downto 11);
    alias fd_sig : std_logic_vector(4 downto 0) is instr(10 downto 6);
    
    
    
	
	signal pc_out_next: std_logic_vector(31 downto 0);
	signal sal_rt_o_rd : std_logic_vector(4 downto 0);
	
	signal srca,srcb,rd2,alu_result,extsig_out : std_logic_vector(31 downto 0);
	signal despl_out,pc_branch,result_mem : std_logic_vector(31 downto 0);
	
	--señales para j
	signal addr32,addr32_corri,addr32_pc_next,pc_next_j : std_logic_vector(31 downto 0);
	
	signal pcsrc : std_logic := '0';
    signal pcstall: std_logic := '0';
    
    -- sinais para FP control unit
    signal is_fp_sig    : std_logic := '0';
    
    -- função simples para converter vector para hex string (versão local)
    function to_hstring_local(v : std_logic_vector) return string is
        constant hexchars : string := "0123456789ABCDEF";
        constant N : integer := v'length / 4;
        variable s : string(1 to N);
        variable nib : std_logic_vector(3 downto 0);
    begin
        for i in 0 to N-1 loop
            nib := v(v'high - i*4 downto v'high - i*4 - 3);
            s(i+1) := hexchars(to_integer(unsigned(nib)) + 1);
        end loop;
        return s;
    end function;

    
    
begin

    -- detectar se é instrução de coprocessador 1 (COP1 / FP)
    -- opcode COP1 = 010001 (binário) = 17 (decimal)
    is_fp_sig <= '1' when code_op = "010001" else '0';
    
	Inst_ControlUnit: ControlUnit PORT MAP(
		OpCode => code_op,
		Funct => funct,
		MemtoReg => memtoreg,
		MemWrite => memwrite,
		Branch => branch,
		AluSrc => alusrc,
		RegDst => regdst,
		RegWrite => regwrite,
		jump => jump,
		AluCtrl => aluctrl
	);

	Inst_PC: PC PORT MAP(
		clk => clk,
        stall => pcstall,
		reset => reset,
		din => pc_next_j,
		dout => pc_out
	);
	
	Inst_Instruction_Memory: Instruction_Memory PORT MAP(
		dir => pc_out,
		instr =>instr
	);
	
    process(instr)
      variable op_i  : integer;
      variable fmt_i : integer;
      variable ft_i  : integer;
      variable fs_i  : integer;
      variable fd_i  : integer;
    begin
      op_i  := to_integer(unsigned(code_op));
      fmt_i := to_integer(unsigned(fmt_sig));
      ft_i  := to_integer(unsigned(ft_sig));
      fs_i  := to_integer(unsigned(fs_sig));
      fd_i  := to_integer(unsigned(fd_sig));

      report "FETCH: instr = 0x" & to_hstring_local(instr)
             & " op=" & integer'image(op_i)
             & " fmt=" & integer'image(fmt_i)
             & " ft=" & integer'image(ft_i)
             & " fs=" & integer'image(fs_i)
             & " fd=" & integer'image(fd_i)
             severity note;
    end process;

    
	ALU_suma_4: ALU_suma PORT MAP(
		a => pc_out,
		b => x"00000004",
		sal => pc_out_next
	);
	
	
	Inst_Mux_rt_o_rd: Mux_2to1_5bits PORT MAP(
		ctrl => regdst,
		a => rt,
		b => rd,
		sal => sal_rt_o_rd
	);
    
    U_FP_CTRL: FP_Control_Unit PORT MAP (
        clk         => clk,
        reset       => reset,
        is_fp       => is_fp_sig,
        fmt         => fmt_sig,
        funct       => funct,        -- você já tem alias funct : instr(5 downto 0)
        fs          => fs_sig,		 --errado!
        ft          => ft_sig,		 --errado!
        fd          => fd_sig,		 --errado!
        probe_index => fp_probe_index,
        probe_data  => fp_probe_data,
        stall       => pcstall
    );
	
	
	Inst_Register_File: Register_File PORT MAP(
		clk => clk,
		we3 => regwrite,
		A1 => rs,
		A2 => rt,
		A3 => sal_rt_o_rd,
		RD1 => srca,
		RD2 => rd2,
		WD3 => result_mem,
        probe_index => rf_probe_index, -- conecta o probe_index externo ao RF
        probe_data  => rf_probe_data   -- expõe o probe_data para fora
	);
	
	writedata<=rd2;
	
	Inst_ALU: ALU PORT MAP(
		a => srca,
		b => srcb,
		func => aluctrl,
		zero=> zero,
		rslt => alu_result
	);
	
	
	Inst_SignExtend: SignExtend PORT MAP(
		din => inmd,
		dout => extsig_out
	);
	
	Inst_Mux_extSign_o_red2: Mux_2to1_32b PORT MAP(
		ctrl => alusrc,
		A => rd2,
		B => extsig_out,
		O => srcb
	);
	
	Inst_corrimiento: corrimiento PORT MAP(
		din => extsig_out,
		dout => despl_out
	);
	
	
	ALU_suma_desplaz: ALU_suma PORT MAP(
		a => despl_out,
		b => pc_out_next,
		sal => pc_branch
	);
	
	pcsrc<=branch and zero;
	
	Inst2_Mux_2to1_32b: Mux_2to1_32b PORT MAP(
		ctrl => pcsrc,
		A => pc_out_next,
		B => pc_branch,
		O => pc_in
	);
	

	Inst3_Mux_2to1_32b: Mux_2to1_32b PORT MAP(
		ctrl => memtoreg,
		A => alu_result,
		B => readdata,
		O => result_mem
	);

	address<=alu_result;
	
	addr32_pc_next<= pc_out_next (31 downto 28) & addr &"00";
	
	Mux_instru_j: Mux_2to1_32b PORT MAP(
		ctrl => jump,
		A => pc_in,
		B => addr32_pc_next,
		O => pc_next_j
	);
	
end Behavioral;
