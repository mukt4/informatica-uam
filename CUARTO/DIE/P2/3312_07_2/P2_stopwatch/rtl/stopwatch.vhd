library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

use work.stopwatch_pkg.all;   -- use project's package
-- Declaracion de la entidad
entity stopwatch is
  generic(
    FAST_SIMULATION : boolean := false
  );
  port(
    -- Input
    Clk       : in std_logic; -- Declaracion del reloj de tipo std_logic
    StartStop : in std_logic; -- Declaracion de le entrada StartStop de tipo std_logic
    Lap       : in std_logic; -- Declaracion de la entrada que marca vuelta de tipo std_logic
    SetToZero : in std_logic; -- Declaracion de la entrada que setea a zero el contador de tipo std_logic
    ResetX    : in std_logic; -- Declaracion del reset de tipo std_logic
    
    -- Output
    SClk : out std_logic; 
    RClk : out std_logic;
    Ser  : out std_logic --Salida que sirve para mandar los valores que tienen que ser mostrados en pantalla
                         -- al display
  );
end stopwatch;

-- Declaracion de arquitectura
architecture stopwatch_arch of stopwatch is
  -- Signals
  -- Declaracion de seniales para enlazar con el resto de entidades
  -- 1 bit signals
  signal s_Reset         : std_logic; -- Senial para el reste
  signal s_ClearCount    : std_logic; -- Senial para la entrada clearCount
  signal s_EnableAux     : std_logic; -- Salida de freq_divider
  signal s_Enable        : std_logic; -- Senial para el enable
  signal s_EnableCount0  : std_logic; -- Senial para el enable de decimas
  signal s_EnableCount1  : std_logic; -- Senial para el enable del contador de segundos
  signal s_EnableCount2  : std_logic; -- Senial para el enable del contador de decenas
  signal s_StartStop     : std_logic; -- Senial apra la entrada StartStop
  signal s_Lap           : std_logic; -- Senial para la entrada Lap
  signal s_SetToZero     : std_logic; -- Senial para la entrada SetToZerro
  signal s_UpdateDisplay : std_logic; -- Senial para el displays
  
  -- Vector signals
  signal s_CountTenths   : std_logic_vector (3 downto 0); -- Vecto que contiene las decimas
  signal s_CountSeconds  : std_logic_vector (3 downto 0); -- Vector que contiene los segundos
  signal s_CountTens     : std_logic_vector (2 downto 0); -- Vector que contiene las decenas
  
  -- Components
  -- Declaracion de componente freq_divider
  component freq_divider
    port(
      Reset     : in std_logic; -- Reset asincrono
      Clk       : in std_logic; -- Reloj
      SyncReset : in std_logic; -- Reset sincrono
      EnableOut : out std_logic -- Enable de salida
    );
  end component;
  
  -- Declaracion de contador
  component counter
    -- Se establece el valor de count_length generico para despues especificar en funcion del tipo de contador
    generic (
      COUNT_LENGTH : integer := 100 -- Count 0 to (COUNT_LENGTH-1)
    );
    port (
      -- Inputs:
      Clk       : in std_logic;  -- Clock (rising edge)
      Reset     : in std_logic;  -- Async reset, active high
      SyncReset : in std_logic;  -- Sync reset, active high
      EnableIn  : in std_logic;  -- Clock enable. If 0 count is stopped
      
      -- Outputs:
      Count     : out std_logic_vector (log2_ceil(COUNT_LENGTH)-1 downto 0);
                                -- Output count value
      EnableOut : out std_logic -- '1' when Count is last value and EnableIn
                                --   is '1' (combinational output)
    );
  end component;
  
  -- Declaracion del componente debounce
  component debounce
    port ( 
      Clk      : in  std_logic;  -- Clock (rising edge)
      Reset    : in  std_logic;  -- Async reset, active high
      EnSample : in  std_logic;  -- Sample DataIn when high
      DataIn   : in  std_logic;  -- Data input
      DataOut  : out std_logic   -- Filtered data output
    );
  end component;
  
  -- Declaracion del componente rst_adapt
  component rst_adapt
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
  end component;
  
  -- Declaracion del componente stopwath_fsm
  component stopwatch_fsm
    port (
      Clk            : in  std_logic;  -- Clock (rising edge)
      Reset          : in  std_logic;  -- Async reset, active high
      StartStop      : in  std_logic;  -- Start/Stop button 
      Lap            : in  std_logic;  -- Lap button
      SetToZero      : in  std_logic;  -- Reset count button
      EnableCountIn  : in  std_logic;  -- Time base enable pulse train
      EnableCountOut : out std_logic;  -- EnableCountIn or '0', as f(state)
      ClearCount     : out std_logic;  -- When '1' clear all counts
      UpdateDisplay  : out std_logic   -- When '0' freeze displayed count
    );
  end component;
  
  -- Declaracion del componente display
  component display_ctrl is
    port (
      Clk : in STD_LOGIC;                           -- Reloj(Flanco de subida)
      Reset : in STD_LOGIC;                         -- Reset asincrono
      Tenths : in STD_LOGIC_VECTOR ( 3 downto 0 );  -- Contador de decimas
      Seconds : in STD_LOGIC_VECTOR ( 3 downto 0 ); -- Contador de segundos
      Tens : in STD_LOGIC_VECTOR ( 2 downto 0 );    -- Contador de centesimas
      UpdateDisplay : in STD_LOGIC;                 -- Entrada para actualizar display
      SClk : out STD_LOGIC;                 
      RClk : out STD_LOGIC;
      Ser : out STD_LOGIC                           -- Salida que manda los valores al display
    );
  end component;
  
  begin
  
  -- -- Declaracion del componente freq_divider
  i_freq_divider: freq_divider
    port map (
      Reset     => s_Reset,        -- Reset asincrono
      Clk       => Clk,            -- Reloj(Flanco de subida)
      SyncReset => s_ClearCount,   -- Reset sincrono
      EnableOut => s_EnableAux     -- Salida de tipo std_logic
    );
  
  -- Declaracion del componente que actua de contador de decimas
  i_count_tenths: counter
    -- Establecemos el valor maximo de la cuenta en las decimas, que sera 10
    generic map (
      COUNT_LENGTH => 10
    )
    port map (
      Clk       => Clk,             -- Reloj(Flanco de subida)
      Reset     => s_Reset,         -- Reset asincrono
      SyncReset => s_ClearCount,    -- Reset sincrono
      EnableIn  => s_EnableCount0,  -- Enable del contador
      Count     => s_CountTenths,   -- Valor de la cuenta
      EnableOut => s_EnableCount1   -- Enable de la salida
    );
  
  -- Declaracion del componente que actua de contador de segundos
  i_count_seconds: counter 
    -- Establecemos el valor maximo de la cuenta en los segundos, que sera 10
    generic map (
      COUNT_LENGTH => 10
    )
    port map(
      Clk       => Clk,             -- Reloj(Flanco de subida)
      Reset     => s_Reset,         -- Reset asincrono
      SyncReset => s_ClearCount,    -- Reset sincrono
      EnableIn  => s_EnableCount1,  -- Enable del contador
      Count     => s_CountSeconds,  -- Valor de la cuenta
      EnableOut => s_EnableCount2   -- Enable de la salida
    );
  
  -- Declaracion del componente que actua de contador de decimas
  i_count_tens: counter
    generic map (
    -- Establecemos el valor maximo de la cuenta en las decimas, que sera 6
      COUNT_LENGTH => 6
    ) 
    port map(
      Clk       => Clk,             -- Reloj(Flanco de subida)
      Reset     => s_Reset,         -- Reset asincrono
      SyncReset => s_ClearCount,    -- Reset sincrono
      EnableIn  => s_EnableCount2,  -- Enable del contador
      Count     => s_CountTens,     -- Valor de la cuenta
      EnableOut => open             -- Enable de la salida
    );
  
  -- Declaracion del boton startStop
  i_debounce_StartStop : debounce
    port map(
      Clk      => Clk,          -- Reloj(Flanco de subida)
      Reset    => s_Reset,      -- Reset asincrono
      EnSample => s_Enable,     -- Enable del startStop
      DataIn   => StartStop,    -- Valores de entrada
      DataOut  => s_StartStop   -- Valores de salida
    );
  
  -- Declaracion del boton de Lap
  i_debounce_Lap : debounce
    port map(
      Clk      => Clk,          -- Reloj(Flanco de subida)
      Reset    => s_Reset,      -- Reset asincrono
      EnSample => s_Enable,     -- Enable del lap
      DataIn   => Lap,          -- Valores de entrada
      DataOut  => s_Lap         -- Valores de salida
    );
  
  -- Declaracion del boton SetToZero
  i_debounce_SetToZero : debounce
    port map(
      Clk      => Clk,          -- Reloj(Flanco de subida)  
      Reset    => s_Reset,      -- Reset asincrono
      EnSample => s_Enable,     -- Enable del setToZero
      DataIn   => SetToZero,    -- Valores de entrada
      DataOut  => s_SetToZero   -- Valores de salida
    );
  
  -- Declaracion del rst_adapt
  i_rst_adapt : rst_adapt
    port map(
      Clk      => Clk,          -- Reloj(Flanco de subida)  
      ResetIn  => ResetX,       -- Reset asincrono
      ResetOut => s_Reset       -- Valor de salida que actua como reset asincrono del resto de
                                -- los componentes
    );
  
-- 
  i_stopwatch_fsm : stopwatch_fsm
    port map(
      Clk            => Clk,              -- Reloj(Flanco de subida)  
      Reset          => s_Reset,          -- Reset asincrono
      StartStop      => s_StartStop,      -- Valor StartStop
      Lap            => s_Lap,            -- Valor Lap
      SetToZero      => s_SetToZero,      -- Valor setToZero
      EnableCountIn  => s_Enable,         -- Valor enable del stopwatch
      EnableCountOut => s_EnableCount0,   -- Enable que va al contador de decimas
      ClearCount     => s_ClearCount,     -- Valor de clearCount
      UpdateDisplay  => s_UpdateDisplay   -- Valor de actualizador del display
    );
  i_display_ctrl : display_ctrl
    port map(
      Clk           => Clk,               -- Reloj(Flanco de subida) 
      Reset         => s_Reset,           -- Reset asincrono
      Tenths        => s_CountTenths,     -- Valor del contador de decimas
      Seconds       => s_CountSeconds,    -- Valor del contador de segundos
      Tens          => s_CountTens,       -- Valor del contador de decenas
      UpdateDisplay => s_UpdateDisplay,   -- Valor del actualizador del display
      SClk          => SClk,
      RClk          => RClk,
      Ser           => Ser                -- Valor que servira para ir actualizando el display
    );
    
  with FAST_SIMULATION select
    s_Enable <= '1' when true,
                s_EnableAux when others;
  
end stopwatch_arch;
