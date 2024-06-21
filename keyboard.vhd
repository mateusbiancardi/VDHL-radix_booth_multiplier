library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

use IEEE.NUMERIC_STD.ALL;

entity keyboard is
    port (
        clk, reset: in std_logic;
        ps2d, ps2c: in std_logic;
        key_code: out std_logic_vector (7 downto 0);
        k_normal: out std_logic;
        k_press: out std_logic;
        k_done_tick: out std_logic
    );
end keyboard;

architecture arch of keyboard is
    constant F0: std_logic_vector(7 downto 0):="11110000";
    constant E0: std_logic_vector(7 downto 0):= "11100000";
   
    type statetype is (idle, extended, released, done);
    signal state_reg, state_next: statetype;
    signal key_reg, key_next: std_logic_vector(7 downto 0);
    signal normal_reg, normal_next: std_logic;
    signal press_reg, press_next: std_logic;
    signal rx_en, rx_done_tick: std_logic;
    signal dout: std_logic_vector (7 downto 0);
begin
    rx_en <= '1';
    ps2_rx_unit: entity work.ps2rx (arch)
        port map(
            clk => clk,
            reset => reset,
            rx_en => rx_en,
            ps2d => ps2d,
            ps2c => ps2c,
            rx_done_tick => rx_done_tick,
            dout => dout
        );
       
    process (clk, reset)
    begin
        if reset = '1' then
            state_reg <= idle;
            key_reg <= (others => '0');
            normal_reg <= '0';
            press_reg <= '0';
        elsif clk'event and clk = '1' then
            state_reg <= state_next;
            key_reg <= key_next;
            normal_reg <= normal_next;
            press_reg <= press_next;
        end if;
    end process;
   
    process (state_reg, normal_reg, key_reg, press_reg, rx_done_tick, dout)
    begin
        key_next <= key_reg;
        press_next <= press_reg;
        normal_next <= normal_reg;
        state_next <= state_reg;
        k_done_tick <= '0';
       
        case state_reg is
            when idle =>
                if rx_done_tick = '1' then
                    if dout = E0 then
                        normal_next <= '0';
                        state_next <= extended;
                    else
                        normal_next <= '1';
                        if dout = F0 then
                            press_next <= '0';
                            state_next <= released;
                        else
                            press_next <= '1';
                            key_next <= dout;
                            state_next <= done;
                        end if;
                    end if;
                end if;
               
            when extended =>
                if rx_done_tick = '1' then
                    if dout = F0 then
                        press_next <= '0';
                        state_next <= released;
                    else
                        press_next <= '1';
                        key_next <= dout;
                        state_next <= done;
                    end if;
                end if;
               
            when released =>
                if rx_done_tick = '1' then
                    key_next <= dout;
                    state_next <= done;
                end if;
           
            when done =>
                k_done_tick <= '1';
                state_next <= idle;
        end case;
    end process;
   
    key_code <= key_reg;
    k_normal <= normal_reg;
    k_press <= press_reg;
end arch;