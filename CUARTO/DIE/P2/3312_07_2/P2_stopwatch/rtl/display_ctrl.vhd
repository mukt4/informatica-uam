----------------------------------------------------------------------------------
-- Company: Escuela Politecnica Superior. Universidad Autonoma de Madrid
-- Design Name: display_ctrl
-- Project Name: stopwatch
-- Author(s): Tomas Higuera y Francisco Alcudia
-- Description:
--   Transforms a stopwatch count into signals to drive the OHO-DY1 display pmod.
--   The input is provided through 3 buses corresponding to tenths of seconds,
--   seconds and tens of seconds.
--   The output for OHO-DY1 is serial, with a serial shift clock and a parallel
--   load clock.
--   Includes a register to freeze the display if UpdateDisplay input is '0'.
--   The input clock (Clk) is assumed to be 125 MHz (Zybo board). Lower
--   frequencies should be ok, higher could make SClk go above its limits.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use work.stopwatch_pkg.all;
-------------------------------------------------------------------------------
-- Entity
-- Declaracion de entidad
entity display_ctrl is
   port (
      Clk           : in  std_logic;  -- Clock (rising edge)
      Reset         : in  std_logic;  -- Async reset, active high
      Tenths        : in  std_logic_vector (3 downto 0); -- 1/10 s digit
      Seconds       : in  std_logic_vector (3 downto 0); -- 1 s digit
      Tens          : in  std_logic_vector (2 downto 0); -- 10 s digit
      UpdateDisplay : in  std_logic;  -- If '0', keep display frozen
      SClk          : out std_logic; -- DY1 serial clock
      RClk          : out std_logic; -- DY1 parallel clock
      Ser           : out std_logic  -- DY1 serial data
  );
end display_ctrl;

-------------------------------------------------------------------------------
-- Architecture
-- Declaracion de arquitectura del display
architecture Behavioral of display_ctrl is
   -- Function to convert a number to standard 7-segment format:
   function dec_to_7seg (digit  : std_logic_vector (3 downto 0);
                         dPoint : std_logic) return std_logic_vector is
   begin
      case digit is
         when "0000" => return (not dPoint) & "1000000";  --0
         when "0001" => return (not dPoint) & "1111001";  --1
         when "0010" => return (not dPoint) & "0100100";  --2
         when "0011" => return (not dPoint) & "0110000";  --3
         when "0100" => return (not dPoint) & "0011001";  --4
         when "0101" => return (not dPoint) & "0010010";  --5
         when "0110" => return (not dPoint) & "0000010";  --6
         when "0111" => return (not dPoint) & "1111000";  --7
         when "1000" => return (not dPoint) & "0000000";  --8
         when "1001" => return (not dPoint) & "0010000";  --9
         when others => return (not dPoint) & "0000110";  --E, to show error
      end case;
   end function;

   -- Function to map normal bit order ({dp,g,f,...b,a}) to weird DY1 order ({e,d,g,a,c,dp,b,f}):
   function map_segments (segIn  : std_logic_vector (7 downto 0)) return std_logic_vector is
      variable segOut : std_logic_vector (7 downto 0);
   begin
      segOut (0) := segIn (5);
      segOut (1) := segIn (1);
      segOut (2) := segIn (7);
      segOut (3) := segIn (2);
      segOut (4) := segIn (0);
      segOut (5) := segIn (6);
      segOut (6) := segIn (3);
      segOut (7) := segIn (4);
      return segOut;
   end function; 
   
   component counter is
   generic (
      COUNT_LENGTH : integer := 100  -- Count 0 to (COUNT_LENGTH-1)
   );
   port (
      -- Inputs:
      Clk       : in std_logic;  -- Clock (rising edge)
      Reset     : in std_logic;  -- Async reset, active high
      SyncReset : in std_logic;  -- Sync reset, active high
      EnableIn  : in std_logic;  -- Clock enable. If 0 count is stopped
      -- Outputs:
      Count     : out std_logic_vector (log2_ceil(COUNT_LENGTH)-1 downto 0);
                                 -- Output count value
      EnableOut : out std_logic  -- '1' when Count is last value and EnableIn
                                 --   is '1' (combinational output)
   );
   end component;
   
   -- signals
   signal sReg        : std_logic_vector(23 downto 0);
   signal sclk_ready  : std_logic;
   signal nBitsToSend : std_logic_vector(log2_ceil(24)-1 downto 0);
   
   type states is (ST_IDLE, ST_SER, ST_SCLK, ST_RCLK);
   signal state      : states;
   signal next_state : states;
   
   begin
    
      i_sclk_counter : counter
         generic map(
            COUNT_LENGTH => 14
         )
         port map(
            Clk       => Clk,
            Reset     => Reset,
            SyncReset => '0',
            EnableIn  => '1',
            Count     => open,
            EnableOut => sclk_ready
         );
      
      process(state,UpdateDisplay,nBitsToSend,sclk_ready)
      begin
         next_state <= state;
         case state is
            when ST_IDLE =>
               if UpdateDisplay = '1' then
                  next_state <= ST_SER;
               end if;
            when ST_SER  =>
               if nBitsToSend > 0 then
                  next_state <= ST_SCLK;
               else
                  next_state <= ST_RCLK;
               end if;
            when ST_SCLK =>
               if sclk_ready = '1' then
                  next_state <= ST_SER;
               end if;
            when ST_RCLK =>
               next_state <= ST_IDLE;
            when others  => 
               next_state <= ST_IDLE;
         end case;
      end process;
      
      process(Clk, Reset)
      begin
        if Reset = '1' then
           state <= ST_IDLE;
        elsif rising_edge(Clk) then
           state <= next_state;
        end if;
      end process;
      
      process(Clk, Reset)
      begin
         if Reset = '1' then
            nBitsToSend <= (others => '0');
         elsif rising_edge(Clk) then
            if state = ST_IDLE then
               nBitsToSend <= conv_std_logic_vector(24, nBitsToSend'length);
            elsif state = ST_SCLK then
               nBitsToSend <= nBitsToSend - 1;
            end if;
         end if;
      end process;
      
      process(Clk, Reset)
      begin
        if Reset = '1' then
           sReg <= (others => '0');
        elsif rising_edge(Clk) then
           if state = ST_IDLE then
              sReg <= map_segments(dec_to_7seg('0' & Tens, '0'))
                    & map_segments(dec_to_7seg(Seconds, '1'))
                    & map_segments(dec_to_7seg(Tenths, '0'));
           elsif state = ST_SCLK then
              sReg <= sReg(22 downto 0) & '0';
           end if;
        end if;
      end process;
      
      Ser  <= sReg(23);
      SClk <= '1' when (state = ST_SCLK) and (sclk_ready = '1') else '0';
      RClk <= '1' when (state = ST_RCLK) else '0';
end Behavioral;
