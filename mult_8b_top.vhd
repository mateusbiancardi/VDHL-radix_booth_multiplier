library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity mult_8b_top is
    port (
        clk, reset: in  std_logic;
        ps2d, ps2c: in  std_logic;
        tx: out  std_logic;
        led: out std_logic_vector(1 downto 0)
    );
end mult_8b_top;

architecture arch of mult_8b_top is
   constant SP: std_logic_vector(7 downto 0):="00100000";
   -- space in ASCII
   type statetype is (idle, getx, highx, lowx, gety, highy, lowy, mult, await_mult);
   type mult_statetype is (idle, send_x, send_xeq, senda1, senda2, send_y, send_yeq, sendb1, sendb2, 
                         send_xmult, send_star, send_ymult, send_eqmult, sendz1, sendz2, sendz3, sendz4,
                         sendspacex, sendspacey, sendenter);
   signal mult_state_reg, mult_state_next: mult_statetype;
   signal state_reg, state_next: statetype;
   signal scan_data, w_data: std_logic_vector(7 downto 0);
   signal scan_done_tick, wr_uart: std_logic;
   signal ascii_code: std_logic_vector(7 downto 0);
   signal k_normal, k_press, k_done_tick: std_logic;
   constant DVSR: std_logic_vector(10 downto 0) := std_logic_vector(to_unsigned(324, 11));
   signal a_reg, a_next: std_logic_vector(7 downto 0);
   signal temp_hex: std_logic_vector(3 downto 0);
   signal temp_ascii: std_logic_vector(7 downto 0);
   signal b_reg, b_next: std_logic_vector(7 downto 0);
   signal hex_out: std_logic_vector (3 downto 0);
   signal is_number: std_logic;
   signal z_reg, z_next, z_out: std_logic_vector(15 downto 0);
   signal valid_reg, valid_next: std_logic;
   signal start, ready: std_logic;
   signal mult_done_tick: std_logic;
   signal out_enable: std_logic;
    
begin
    led(0) <= valid_reg;
    
    keyboard_unit: entity work.keyboard(arch)
        port map (
            clk => clk,
            reset => reset,
            ps2d => ps2d,
            ps2c => ps2c,
            key_code => scan_data,
            k_normal => k_normal,
            k_press => k_press,
            k_done_tick => scan_done_tick
        ); 
    
    booth_radix4_unit: entity work.booth_radix4(arch)
        port map (
            clk => clk,
            reset => reset,
            a_in => a_reg,
            b_in => b_reg,
            start => start,
            ready => ready,
            done_tick => mult_done_tick,
            p_out => z_out
        );

    uart_unit: entity work.uart(str_arch)
     generic map (
        DBIT => 8,   -- # data bits
        SB_TICK => 16,  -- # ticks for stop bits, 16 per bit
        FIFO_W  => 5    -- # FIFO addr bits (depth: 2^FIFO_W)
     )
     port map(clk=>clk, reset=>reset, rd_uart=>'0', dvsr => DVSR,
               wr_uart=>wr_uart, rx=>'1', w_data=>w_data,
               tx_full=>open, rx_empty=>open, r_data=>open,
               tx=>tx);
               
               
    process (clk, reset)
    begin
        if reset = '1' then
            state_reg <= idle;
            mult_state_reg <= idle;
            a_reg <= "00000000";
            b_reg <= "00000000";
            z_reg <= (others=>'0');
            valid_reg <= '0';
        elsif clk'event and clk = '1' then
            state_reg <= state_next;
            mult_state_reg <= mult_state_next;
            a_reg <= a_next;
            b_reg <= b_next;
            z_reg <= z_next;
            valid_reg <= valid_next;
        end if;
    end process;
    
    process (scan_done_tick, valid_reg,scan_data, a_reg, b_reg, state_reg, mult_done_tick, z_reg, k_normal, k_press, is_number, hex_out, ready, z_out)
    begin
        a_next <= a_reg;
        b_next <= b_reg;
        z_next <= z_reg;
        state_next <= state_reg;
        out_enable <= '0';
        valid_next <= valid_reg;
        start <= '0';
        case state_reg is
            when idle =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' then
                    if scan_data = x"22" then -- x
                        state_next <= getx;
                    elsif scan_data = x"35" then --y
                        state_next <= gety;
                    elsif scan_data = x"1B" then --s
                        state_next <= mult;
                    elsif scan_data = x"44" then --o
                        if valid_reg = '1' then
                           out_enable <= '1';
                        else
                           state_next <= mult;
                        end if;
--                    else
--                        state_next <= idle;
                    end if;
                end if;
             
             when getx =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' then
                    if scan_data = x"55" then--=
                        state_next <= highx;
                    else
                        state_next <= idle;
                    end if;
                end if;
             when gety =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' then
                     if scan_data = x"55" then--=
                         state_next <= highy;
                     else
                         state_next <= idle;
                     end if;
                end if;
             when highx =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' and is_number = '1' then
                      a_next <= hex_out & "0000";
                      valid_next <= '0';
                      state_next <= lowx;
                end if;
             when highy =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' and is_number = '1' then
                      b_next <= hex_out & "0000";
                      valid_next <= '0';
                      state_next <= lowy;
                end if;
             when lowx =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' and is_number = '1' then
                       a_next <= a_reg(7 downto 4) & hex_out;
                       state_next <= idle;
                       valid_next <= '0';
                 end if;
             when lowy =>
                if scan_done_tick = '1' and k_normal = '1' and k_press = '1' and is_number = '1' then
                        b_next <= b_reg(7 downto 4) & hex_out;
                        state_next <= idle;
                        valid_next <= '0';
                  end if;
             when mult =>
                if ready = '1' then
                    start <= '1';
                    state_next <= await_mult; 
                end if;
             when await_mult =>
                if mult_done_tick = '1' then
                    valid_next <= '1';
                    z_next <= z_out;
                    out_enable <= '1';
                    state_next <= idle;
                end if;
        end case;    
    end process;
    
    process (out_enable, mult_state_reg, temp_ascii, a_reg, b_reg, z_reg)
    begin
        mult_state_next <= mult_state_reg;
        wr_uart <= '0';
        w_data <= "00000000";
        temp_hex <= "0000";
        case mult_state_reg is
            when idle =>
                wr_uart <= '0';
                if out_enable = '1' then
--                    mult_state_next <= senda1;
                    mult_state_next <= send_x;
                end if;
            when send_x =>
                mult_state_next <= send_xeq;
                wr_uart <= '1';
                w_data <= "01111000";--x_ascii
            when send_xeq =>
                mult_state_next <= senda1;
                wr_uart <= '1';
                w_data <= "00111101";--equal_ascii
            when senda1 =>
                temp_hex <= a_reg(7 downto 4);
                mult_state_next <= senda2;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when senda2 =>
                temp_hex <= a_reg(3 downto 0);
--                mult_state_next <= sendb1;
                mult_state_next <= sendspacex;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendspacex =>
                mult_state_next <= send_y;
                wr_uart <= '1';
                w_data <= "00100000";--space_ascii            
            when send_y =>
                mult_state_next <= send_yeq;
                wr_uart <= '1';
                w_data <= "01111001";--y_ascii
            when send_yeq =>
                mult_state_next <= sendb1;
                wr_uart <= '1';
                w_data <= "00111101";--equal_ascii
            when sendb1 =>
                temp_hex <= b_reg(7 downto 4);
                mult_state_next <= sendb2;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendb2 =>
                temp_hex<= b_reg(3 downto 0);
--                mult_state_next <= sendz1; send_xmult, send_star, send_ymult, send_eqmult
                mult_state_next <= sendspacey;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendspacey =>
                mult_state_next <= send_xmult;
                wr_uart <= '1';
                w_data <= "00100000";--space_ascii            
            when send_xmult =>
                mult_state_next <= send_star;
                wr_uart <= '1';
                w_data <= "01111000";
            when send_star =>
                mult_state_next <= send_ymult;
                wr_uart <= '1';
                w_data <= "00101010";
            when send_ymult =>
                mult_state_next <= send_eqmult;
                wr_uart <= '1';
                w_data <= "01111001";
            when send_eqmult =>
                mult_state_next <= sendz1;
                wr_uart <= '1';
                w_data <= "00111101";             
            when sendz1 =>
                temp_hex <= z_reg(15 downto 12);
                mult_state_next <= sendz2;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendz2 =>
                temp_hex <= z_reg(11 downto 8);
                mult_state_next <= sendz3;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendz3 =>
                temp_hex <= z_reg(7 downto 4);
                mult_state_next <= sendz4;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendz4 =>
                temp_hex <= z_reg(3 downto 0);
                mult_state_next <= sendenter;
                wr_uart <= '1';
                w_data <= temp_ascii;
            when sendenter =>
                mult_state_next <= idle;
                wr_uart <= '1';
                w_data <= "00111011";--';'_ascii
        end case;
        
    end process;

    process (scan_data) --pega o valor digitado e transforma de makecode para hexadecimal
    begin
        case scan_data is
            when x"45" =>
                hex_out <= "0000";
                is_number <= '1';
            when x"16" =>
                hex_out <= "0001";
                is_number <= '1';
            when x"1E" =>
                hex_out <= "0010";
                is_number <= '1';
            when x"26" =>
                hex_out <= "0011";
                is_number <= '1';
            when x"25" =>
                hex_out <= "0100";
                is_number <= '1';
            when x"2E" =>
                hex_out <= "0101";
                is_number <= '1';
            when x"2D" =>
                hex_out <= "0110";
                is_number <= '1';
            when x"19" =>
                hex_out <= "0111";
                is_number <= '1';
            when x"1C" =>
                hex_out <= "1000";
                is_number <= '1';
            when x"1A" =>
                hex_out <= "1001";
                is_number <= '1';
            when x"20" =>
                hex_out <= "1010";
                is_number <= '1';
            when x"21" =>
                hex_out <= "1011";
                is_number <= '1';
            when x"22" =>
                hex_out <= "1100";
                is_number <= '1';
            when x"2A" =>
                hex_out <= "1101";
                is_number <= '1';
            when x"2B" =>
                hex_out <= "1110";
                is_number <= '1';
            when x"29" =>
                hex_out <= "1111";
                is_number <= '1';
            when others =>
                hex_out <= "0000";
                is_number <= '0';
       end case;
    end process;
    
    process(temp_hex) --transforma de hexa para ascii
    begin
        case temp_hex is
            when "0000" =>
                temp_ascii <= "00110000";
            when "0001" =>
                temp_ascii <= "00110001";
            when "0010" =>
                temp_ascii <= "00110010";
            when "0011" =>
                temp_ascii <= "00110011";
            when "0100" =>
                temp_ascii <= "00110100";
            when "0101" =>
                temp_ascii <= "00110101";
            when "0110" =>
                temp_ascii <= "00110110";
            when "0111" =>
                temp_ascii <= "00110111";
            when "1000" =>
                temp_ascii <= "00111000";
            when "1001" =>
                temp_ascii <= "00111001";
            when "1010" =>
                temp_ascii <= "01000001";
            when "1011" =>
                temp_ascii <= "01000010";
            when "1100" =>
                temp_ascii <= "01000011";
            when "1101" =>
                temp_ascii <= "01000100";
            when "1110" =>
                temp_ascii <= "01000101";
            when others=> --"1111" =>
                temp_ascii <= "01000110";
        end case;
    end process;
end arch;