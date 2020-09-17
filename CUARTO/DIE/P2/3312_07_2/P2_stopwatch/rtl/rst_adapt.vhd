--------------------------------------------------------------------------------
-- Escuela Politecnica Superior. Universidad Autonoma de Madrid.
-- Design Name: rst_adapt
-- Project Name: stopwatch
-- Author: Fernando Barbero, UAM
-- Description:
--   Synchronizes a external reset signal to generate the internal reset
--   signal, free of possible metastability and synchronous with the clock.
--   Reset assertion is asynchronous, deassertion is synchronous.
--------------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

--------------------------------------------------------------------------------
-- Entity declaration
--------------------------------------------------------------------------------
-- Declaracion de la entidad
entity rst_adapt is
   generic (
      META_DEPTH : integer := 3  -- Number of flops in anti-metastability
                                 --   chain. Optimum configuration
                                 --   depends on clock frequency.
   );
   port (
   -- Clock and external reset inputs:
      Clk      : in  std_logic;
      ResetIn  : in  std_logic;
      -- Synchronized reset output:
      ResetOut : out std_logic
   );
end rst_adapt;

--------------------------------------------------------------------------------
-- Architecture
--------------------------------------------------------------------------------
-- Declaracion de la arquitectura
architecture rtl of rst_adapt is
   -- Signal declaration for anti-metastability flip-flop chain:
   signal resetMeta : std_logic_vector (META_DEPTH-1 downto 0);
begin
  
   -- Anti-metastability (shift reset bit in):
   -- Declaracion del proceso
   process (Clk)
   begin
      -- Si es un flanco de subida de reloj
      if Clk'event and Clk = '1' then
         resetMeta <= resetMeta(META_DEPTH-2 downto 0) & ResetIn;
      end if;
   end process;

   -- Assertion is asynchronous, deassertion is synchronous:

   ResetOut <= ResetIn or resetMeta(META_DEPTH-1);
    
end rtl;

