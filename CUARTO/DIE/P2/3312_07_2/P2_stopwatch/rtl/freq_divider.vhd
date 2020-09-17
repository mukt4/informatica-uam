library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-- Declaracion de la entidad freq_divider
entity freq_divider is
  --Declaracion de los puertos
  port(
    -- Reset de tipo std_logic
    Reset     : in std_logic;
    -- Reloj de tipo std_logic
    Clk       : in std_logic;
    -- Reset sincrono
    SyncReset : in std_logic;
    
    -- Enable de la salida
    EnableOut : out std_logic
  );    
end freq_divider;

-- Declaracion de la arquitectura
architecture freq_divider_arch of freq_divider is
  -- Declaracion de constante al alcanzar la frecuencia maxima
  constant MAXIMO: integer := 12499999;
  -- Declaracion de senial para llevar la cuenta de la frecuencia
  signal count : std_logic_vector(23 downto 0);
    
  begin
  -- Declaracion del proceso
  process(Clk, Reset)
  begin
    -- Reset asincrono a nivel alto que pone el contador, asi como la salida a 0
    if Reset = '1' then
      count <= (others=> '0');
      EnableOut <= '0';
    elsif rising_edge(Clk) then
      -- Reset sincrono a nivel alto que pone el contador, asi como la salida a 0
      if SyncReset = '1' then
        count <= (others=> '0');
        EnableOut <= '0';
      -- Si el reeset no esta activado, pero se ha llegado al valor maximo de la cuenta, se inicializan
      -- los valores de la salida y del contador a 0
      elsif conv_integer(count) = MAXIMO then
        count <= (others=> '0');
        EnableOut <= '1';
      -- Si no se ha llegado al maximo se suma uno a la cuenta
      else
        count <= count + 1;
        EnableOut <= '0';
      end if;
    end if;
  end process;
end freq_divider_arch;
