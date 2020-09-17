----------------------------------------------------------------------------------
-- Escuela Politecnica Superior. Universidad Autonoma de Madrid.
-- Design Name: debounce
-- Project Name: stopwatch
-- Author: Fernando Barbero, UAM
-- Description: 
--   Provides a very basic anti-bouncing filtering of an input signal. The
--   filtering is based on updating sample values at a rate defined by EnSample.
--   Any bounce between two EnSample pulses will not be passed to the output.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-------------------------------------------------------------------------------
-- Entity
-- Declaracion de entidad
entity debounce is
   port ( 
      Clk      : in  std_logic;  -- Clock (rising edge)
      Reset    : in  std_logic;  -- Async reset, active high
      EnSample : in  std_logic;  -- Sample DataIn when high
      DataIn   : in  std_logic;  -- Data input
      DataOut  : out std_logic   -- Filtered data output
   );
end debounce;

-------------------------------------------------------------------------------
-- Architecture
-- Declaracion de arquitectura
architecture Behavioral of debounce is

   -- Anti-metastability FF chain for the input:
   signal dataInMeta : std_logic_vector (1 downto 0);

begin
   -- Declaracion de proceso
   process (Clk, Reset)
   begin
      -- Reset asincrono a nivel alto que inicializa el valor de dataIn
      if Reset = '1' then
         dataInMeta <= (others => '0');
      elsif rising_edge (Clk) then
         -- En flanco de subida de relok, seteamos el valor de dataInMeta concatenado con DataIn
         dataInMeta <= dataInMeta(0) & DataIn;
         -- Si el enable esta a 1 seteamos dataOut
         if EnSample = '1' then
            -- Guardamos en DataOut el valor de dataIn
            DataOut <= dataInMeta(1);    
         end if;
      end if;
   end process;

end Behavioral;

