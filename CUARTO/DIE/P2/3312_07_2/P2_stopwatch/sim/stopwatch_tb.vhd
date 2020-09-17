----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 11/07/2019 12:37:57 PM
-- Design Name: 
-- Module Name: stopwatch_tb - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity stopwatch_tb is
--  Port ( );
end stopwatch_tb;

architecture Behavioral of stopwatch_tb is

  component stopwatch
    generic(
      FAST_SIMULATION : boolean := false
    );
    port(
      -- Input
      Clk       : in std_logic;
      StartStop : in std_logic;
      Lap       : in std_logic;
      SetToZero : in std_logic;
      ResetX    : in std_logic;
      
      -- Output
      SClk : out std_logic;
      RClk : out std_logic;
      Ser  : out std_logic
    );
  end component;
  
  constant CLK_PERIOD : time := 8 ns;
  constant SIM_TIME   : time := 70 us;
    
  signal clk, startStop, lap, setToZero, resetX, sClk, rClk, ser : std_logic;
  signal endSimulation : boolean := false;
  
begin

  uut: stopwatch
    generic map(
      FAST_SIMULATION => true
    )
    port map(
      -- Input
      Clk       => clk,
      StartStop => startStop,
      Lap       => lap,
      SetToZero => setToZero,
      ResetX    => resetX,
      
      -- Output
      SClk => sClk,
      RClk => rClk,
      Ser  => ser
    );
  
  process
  begin
    if endSimulation = false then
      clk <= '1';
      wait for CLK_PERIOD/2;
      clk <= '0';
      wait for CLK_PERIOD/2;
    else
      wait;
    end if;
  end process;
  
  process
  begin
    startStop <= '0';
    lap       <= '0';
    setToZero <= '0';
    resetX    <= '1';
    
    wait for CLK_PERIOD * 5;
    
    resetX    <= '0';
    startStop <= '1';
    
    wait for 1000 ns;
    
    startStop <= '0';
    
    wait for SIM_TIME;
    
    endSimulation <= true;
    
    wait;
  end process;
  
  
end Behavioral;
