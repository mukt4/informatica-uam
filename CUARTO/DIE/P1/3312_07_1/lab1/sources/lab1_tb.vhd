library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

entity lab1_tb is
end lab1_tb;

architecture tb of lab1_tb is

   component lab1 is
      port (
         swt : in  std_logic_vector (3 downto 0);
         led : out std_logic_vector (3 downto 0)
      );
   end component lab1;
   
   signal switches : std_logic_vector (3 downto 0);
   signal leds     : std_logic_vector (3 downto 0);

   function expected_leds (swt : in std_logic_vector (3 downto 0)) return std_logic_vector is
      variable v_expected_leds : std_logic_vector (3 downto 0);
   begin      
      v_expected_leds(0) := not swt(0);
      v_expected_leds(1) := swt(1) and not swt(2);
      v_expected_leds(3) := swt(2) and swt(3);
      v_expected_leds(2) := v_expected_leds(1) or v_expected_leds(3);
      return v_expected_leds;
   end function;

begin
   
   dut : lab1 port map (
      led => leds,
      swt => switches
   );

   
end architecture;
