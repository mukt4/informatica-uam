--------------------------------------------------------------
-- Simple Example of a L bit adder regitered input and output
-- Orig Version G.Sutter 2004
-- Rev for Timing-Exercise in Vivado 2018.2 sep2018.
--------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity sumador_reg is
    generic(L : integer := 32);
    Port (  clk : in std_logic;
            cin : in std_logic;
            a : in std_logic_vector(L-1 downto 0);
            b : in std_logic_vector(L-1 downto 0);
            s : out std_logic_vector(L-1 downto 0);
            cout : out std_logic);
end sumador_reg;

architecture Behavioral of sumador_reg is

signal i_cin : std_logic;
signal i_a, i_b : std_logic_vector(L-1 downto 0);
signal i_s : std_logic_vector(L downto 0);

begin

    registers: process(clk)
    begin
       if CLK'event and CLK='1' then  --CLK rising edge
            --input registers
            i_a <= a;
            i_b <= b;
            i_cin <= cin;
            --output registers
            s <= i_s(L-1 downto 0);
            cout <= i_s(L);
       end if;
    end process;
    
    i_s <= ('0' & i_a) + i_b + i_cin;

end Behavioral;
