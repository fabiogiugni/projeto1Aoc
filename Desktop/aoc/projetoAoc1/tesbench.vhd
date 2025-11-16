library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity MIPS_tb is
end MIPS_tb;

architecture tb of MIPS_tb is

    component MIPS is
        Port ( clk      : in  STD_LOGIC;
               reset    : in  STD_LOGIC;
               memwrite : out STD_LOGIC;
               readdata : in  STD_LOGIC_VECTOR (31 downto 0);
               address  : out STD_LOGIC_VECTOR (31 downto 0);
               writedata: out STD_LOGIC_VECTOR (31 downto 0);
               rf_probe_index : in  std_logic_vector(4 downto 0);
               rf_probe_data  : out std_logic_vector(31 downto 0);

               -- ADICIONADAS: portas para probe do FP RF (verifique nomes na entity MIPS)
               fp_probe_index : in  std_logic_vector(4 downto 0);
               fp_probe_data  : out std_logic_vector(31 downto 0)
        );
    end component;

    signal clk_tb      : std_logic := '0';
    signal reset_tb    : std_logic := '1';
    signal memwrite_tb : std_logic;
    signal readdata_tb : std_logic_vector(31 downto 0) := (others => '0');
    signal address_tb  : std_logic_vector(31 downto 0);
    signal writedata_tb: std_logic_vector(31 downto 0);

    -- sinais de probe para ler o RF interno (inteiros)
    signal probe_index_tb : std_logic_vector(4 downto 0) := (others => '0');
    signal probe_data_tb  : std_logic_vector(31 downto 0);

    -- sinais de probe para ler o FP RF (adicionados)
    signal fp_probe_index_tb : std_logic_vector(4 downto 0) := (others => '0');
    signal fp_probe_data_tb  : std_logic_vector(31 downto 0);

    constant clk_period : time := 10 ns;

    ------------------------------------------------------------------
    -- util: converte 32-bit std_logic_vector para string hexadecimal
    ------------------------------------------------------------------
    function to_hstring(v : std_logic_vector) return string is
        constant hexchars : string := "0123456789ABCDEF";
        constant N : integer := v'length / 4;
        variable s : string(1 to N);
        variable high_idx : integer;
        variable low_idx  : integer;
        variable nibble    : std_logic_vector(3 downto 0);
        variable val_int   : integer;
    begin
        for j in 0 to N-1 loop
            high_idx := v'high - j*4;
            low_idx  := high_idx - 3;
            nibble := v(high_idx downto low_idx);
            val_int := to_integer(unsigned(nibble));
            s(j+1) := hexchars(val_int + 1);
        end loop;
        return s;
    end function;

    -- util: mostra vetores arbitrários como '0'/'1'/'X'
    function vec_to_string(v: std_logic_vector) return string is
        variable s : string(1 to v'length);
        variable idx : integer := 1;
    begin
        for i in v'range loop
            if v(i) = '1' then
                s(idx) := '1';
            elsif v(i) = '0' then
                s(idx) := '0';
            elsif v(i) = 'X' then
                s(idx) := 'X';
            elsif v(i) = 'Z' then
                s(idx) := 'Z';
            else
                s(idx) := '?';
            end if;
            idx := idx + 1;
        end loop;
        return s;
    end function;

begin

    uut: MIPS
        port map (
            clk => clk_tb,
            reset => reset_tb,
            memwrite => memwrite_tb,
            readdata => readdata_tb,
            address => address_tb,
            writedata => writedata_tb,
            rf_probe_index => probe_index_tb,
            rf_probe_data  => probe_data_tb,

            -- mapeamento dos novos sinais de probe FP
            fp_probe_index => fp_probe_index_tb,
            fp_probe_data  => fp_probe_data_tb
        );

    -- clock generator
    clk_proc: process
    begin
        while now < 1200 ns loop
            clk_tb <= '0';
            wait for clk_period / 2;
            clk_tb <= '1';
            wait for clk_period / 2;
        end loop;
        wait;
    end process;

    -- stimulus + monitor
    stim_proc: process
        variable cycle : integer := 0;
        constant RUN_CYCLES : integer := 100; -- ajuste se quiser rodar mais
        variable i : integer;
    begin
        -- reset pulse
        reset_tb <= '1';
        wait for 3 * clk_period;
        reset_tb <= '0';
        wait for clk_period;
        

        -- run for RUN_CYCLES rising edges, printing observed outputs
        for i in 1 to RUN_CYCLES loop
            wait until rising_edge(clk_tb);
            cycle := cycle + 1;
        end loop;

        report "MIPS_tb: finished running " & integer'image(RUN_CYCLES) & " cycles." severity note;

        -- imprime estado final dos registradores inteiros usando o probe
        report "=== ESTADO FINAL DOS REGISTRADORES (via probe inteiro) ===";
        for i in 0 to 31 loop
            probe_index_tb <= std_logic_vector(to_unsigned(i,5));
            wait for 1 ns; -- tempo para o RF atualizar probe_data
            report "R" & integer'image(i) & " = 0x" & to_hstring(probe_data_tb);
        end loop;

        -- imprime estado final dos registradores de ponto flutuante (FP RF) usando o probe FP
        report "=== ESTADO FINAL DOS REGISTRADORES DE PONTO FLUTUANTE (via FP probe) ===";
        for i in 0 to 31 loop
            fp_probe_index_tb <= std_logic_vector(to_unsigned(i,5));
            wait for 1 ns; -- tempo para o FP RF atualizar fp_probe_data
            report "F" & integer'image(i) & " = 0x" & to_hstring(fp_probe_data_tb);
        end loop;

        report "MIPS_tb: done." severity note;
        wait;
    end process;

end tb;
