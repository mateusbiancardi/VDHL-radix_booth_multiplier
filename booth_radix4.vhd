library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;

entity booth_radix4 is
    port (
        clk, reset: in std_logic;
        a_in, b_in: in std_logic_vector(7 downto 0);
        start: in std_logic;
        ready, done_tick: out std_logic;
        p_out: out std_logic_vector(15 downto 0)
    );
end booth_radix4;

architecture arch of booth_radix4 is
    type state_type is (idle, mult, done);
    signal state_reg, state_next: state_type;
    signal b_ext_reg, b_ext_next: std_logic_vector(8 downto 0);
    signal a_reg, a_next: unsigned(15 downto 0);
    signal p_reg, p_next: unsigned(15 downto 0);
    signal k_reg, k_next: unsigned(2 downto 0);
    alias triplet: std_logic_vector(2 downto 0)is b_ext_reg(2 downto 0);
    signal aux: unsigned(15 downto 0);
begin

    process (triplet, a_reg)
    begin
        case triplet is
            when "000" => aux <= (others => '0');
            when "001" | "010" => aux <= a_reg;
            when "011" => aux <= a_reg(14 downto 0) & '0';
            when "100" => aux <= not (a_reg(14 downto 0) & '0') + 1;
            when "101" | "110" => aux <= not(a_reg)+1;
            when others => aux <= (others => '0');
        end case;
    end process;
   
    process (clk, reset)
    begin
        if reset = '1' then
            state_reg <= idle;
            a_reg <= (others => '0');
            b_ext_reg <= (others => '0');
            p_reg <= (others => '0');
            k_reg <= (others => '0');
        elsif clk'event and clk = '1' then
            state_reg <= state_next;
            a_reg <= a_next;
            b_ext_reg <= b_ext_next;
            p_reg <= p_next;
            k_reg <= k_next;
        end if;
    end process;
   
    p_out <= std_logic_vector(p_reg);
    process (state_reg, a_reg, b_ext_reg, p_reg, k_reg, start, a_in, b_in, aux)
    begin
        state_next <= state_reg;
        a_next <= a_reg;
        b_ext_next <= b_ext_reg;
        p_next <= p_reg;
        k_next <= k_reg;
        ready <= '0';
        done_tick <= '0';
       
        case state_reg is
            when idle =>
                ready <= '1';
                if start = '1' then
                    a_next(15 downto 8) <= (others => '0');
                    a_next(7 downto 0) <= unsigned(a_in);
                    b_ext_next <= b_in & '0';
                    p_next <= (others => '0');
                    k_next <= (others => '0');
                    state_next <= mult;
                end if;
            when mult =>
                p_next <= p_reg + aux;
                a_next <= a_reg(13 downto 0) & "00";
                b_ext_next <= "00" & b_ext_reg(8 downto 2);
                k_next <= k_reg + 1;
                if k_reg = 4 then
                    state_next <= done;
                end if;
            when done =>
                done_tick <= '1';
                state_next <= idle;
        end case;
    end process;
   
end arch;