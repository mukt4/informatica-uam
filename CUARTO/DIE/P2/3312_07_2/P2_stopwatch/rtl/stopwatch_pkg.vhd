----------------------------------------------------------------------------------
-- Escuela Politecnica Superior. Universidad Autonoma de Madrid.
-- Design Name: stopwatch_pkg (package)
-- Project Name: stopwatch
-- Author: Fernando Barbero, UAM
-- Description: 
--   Provides auxiliary functions, etc. for stopwatch project.
----------------------------------------------------------------------------------

library IEEE;
use IEEE.STD_LOGIC_1164.all;

package stopwatch_pkg is

   -- Get the logarithm in base 2 of the given integer, rounded up to the next
   --  integer (log2_ceil(8) = log2_ceil(7) = 3):
   function log2_ceil (constant i_number : in integer  ) return integer;

end stopwatch_pkg;


package body stopwatch_pkg is

   function log2_ceil (constant i_number : in integer  ) return integer is
      variable l_aux : integer;
   begin
      l_aux := 0;
      if i_number > 1 then
         while i_number > 2**l_aux loop
            l_aux := l_aux + 1;
         end loop;
      else
         l_aux := 1;
      end if;
      return l_aux; 
   end log2_ceil;
 
end stopwatch_pkg;
