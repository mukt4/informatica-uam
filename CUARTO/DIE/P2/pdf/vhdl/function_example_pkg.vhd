package function_example_pkg is

-- Example of a function with 2 arguments: returns the greatest of the two.
-- Function declaration:
function max (valueA : integer; valueB  : integer) return integer;

end package function_example_pkg;


package body function_example_pkg is

-- Function body for the function declared above:
function max (valueA : integer; valueB  : integer) return integer is
begin
   if valueA > valueB then
      return valueA;
   else
      return valueB;
   end if;
end max;

end package body function_example_pkg;
