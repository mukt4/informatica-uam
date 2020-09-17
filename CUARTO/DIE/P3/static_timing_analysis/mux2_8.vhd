-----------------------------------------------------
-- Simple Example of a 8 bit 2-to-1 mux
-- Orig Version G.Sutter 2004
-- Rev for Timing-Exercise in Vivado 2018.2 sep2018.
-----------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity mux2_8 is
    Port ( sel : in std_logic;
	 	   a : in std_logic_vector(7 downto 0);
           b : in std_logic_vector(7 downto 0);
           s : out std_logic_vector(7 downto 0));
end mux2_8;

architecture Behavioral of mux2_8 is
begin

s <= a when sel = '1' else b;

end Behavioral;
