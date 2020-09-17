-----------------------------------------------------
-- Simple Example of a 1 bit 8-to-1 mux
-- Orig Version G.Sutter 2004
-- Rev for Timing-Exercise in Vivado 2018.2 sep2018.
-----------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity mux8_1 is
    Port ( entradas : in std_logic_vector(7 downto 0);
           selec : in std_logic_vector(2 downto 0);
           salida : out std_logic);
end mux8_1;

architecture Behavioral of mux8_1 is

begin

  salida <= entradas(conv_integer(selec));

end Behavioral;
