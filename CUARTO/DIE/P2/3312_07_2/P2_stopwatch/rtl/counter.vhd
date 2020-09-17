----------------------------------------------------------------------------------
-- Escuela Politecnica Superior. Universidad Autonoma de Madrid.
-- Design Name: counter
-- Project Name: stopwatch
-- Author(s): Tomas Higuera y Francisco Alcudia
-- Description: 
--   Parameterizable counter. Being the value of the generic COUNT_LENGTH,
--   the counter counts from 0 up to (COUNT_LENGTH-1).
--   The size of the output count port is derived automatically from
--   the generic.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use work.stopwatch_pkg.all;   -- use project's package

-------------------------------------------------------------------------------
-- Entity
-- Declaracion de la entidad del contador
entity counter is
   -- Declaracion de variable para realizar contadores genericos
   generic (
      COUNT_LENGTH : integer := 100  -- Count 0 to (COUNT_LENGTH-1)
   );
   -- Declaracion de puertos
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
end counter;

-------------------------------------------------------------------------------
-- Architecture
-- Declaracion de la arquitectura del contador
architecture Behavioral of counter is
   -- Internal signal to implement the count:
   signal iCount     : std_logic_vector (log2_ceil(COUNT_LENGTH) - 1 downto 0);
   -- Following signal is '1' if iCount = max value (according to COUNT_LENGTH):
   signal maxReached : std_logic;

begin

   -- Set maxReached when final count value is reached:

   -- Bit que se pone a 1 cuando el contador alcanza el ultimo valor de la cuenta
   maxReached <= '1' when iCount = (COUNT_LENGTH - 1) else
                 '0';

   -- Implement the counter (iCount). Reset with maxReached:

    -- Declaracion del proceso del flip flop con reset asincrono 
    process(Clk, Reset)
    begin
        -- Reset asincrono a nivel alto que inicializa el contador a 0
        if Reset = '1' then
            iCount <= (others=> '0');
         elsif rising_edge(Clk) then
            -- Reset sincrono que inicializa el contador a 0
            if SyncReset = '1' then
                iCount <= (others=> '0');
            -- Enable que determina si el contador esta en funcionamiento
            elsif EnableIn = '1' then
                -- El contador al alcnzar el maximo se resetea a 0
                if maxReached = '1' then
                    iCount <= (others=> '0');
                -- Si el contador no ha alcanzado el maximo, sigue aumentando
                else
                    iCount <= iCount + 1;
                end if;
            end if;
         end if;
    end process;
   -- Assign outputs: Count and EnableOut (active when EnableIn and maxReached):
   
    -- Asignacion del count con el contador interno
    Count <= iCount;
    -- Asignacion de enableOut cuando se alcanza el final de cuenta
    EnableOut <= '1' when maxReached = '1' and EnableIn = '1' else
                '0';

end Behavioral;

