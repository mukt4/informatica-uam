library ieee;
use ieee.std_logic_1164.all;

entity lab1 is
   port (
      swt : in  std_logic_vector (3 downto 0);
      led : out std_logic_vector (3 downto 0)
   );
end lab1;

architecture rtl of lab1 is
   signal ledSig : std_logic_vector (3 downto 0);
begin




   led <= ledSig;
   
end architecture;
