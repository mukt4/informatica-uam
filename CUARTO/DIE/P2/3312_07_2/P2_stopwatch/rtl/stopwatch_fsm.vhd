----------------------------------------------------------------------------------
-- Escuela Politecnica Superior. Universidad Autonoma de Madrid.
-- Design Name: stopwatch_fsm
-- Project Name: stopwatch
-- Author(s): Tomas Higuera y Francisco Alcudia
-- Description: 
--   State machine controlling the stopwatch's operation. The stopwatch
--   can be idle (00:00), running, showing the frozen time for a lap or 
--   stopped.
--   The block has two outputs:
--     - EnableCountOut: activated ('1') while running or showing lap time,
--       but only if EnableCountIn is active (this input signal can be
--       used as an enable from the time base counter).
--     - UpdateDisplay: when '1' the time should be shown. It is '0'
--       to freeze the display with the stopwatch running, while showing
--       a lap's time, or if the stopwatch is stopped.
--   The input button signals are assumed to be already debounced. In this
--   block the transition from '0' to '1' of these signals is used to
--   change state.
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

-------------------------------------------------------------------------------
-- Entity
-- Declaracion de la entidad
entity stopwatch_fsm is
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
end stopwatch_fsm;

-------------------------------------------------------------------------------
-- Architecture
-- Declaracion de la arquitectura
architecture Behavioral of stopwatch_fsm is

   -- Type for the different states:

   -- Declaracion de los estados de la maquina
   type estados is (IDLE,RUNNING,STOPPED,SHOWLAP);
 
   -- Current state and next state

   -- Declaracion de seniales que marcaran el estado en el que se encuentra la maquina
   signal state         : estados;
   signal nextState     : estados;
  
   -- Registered versions of button signals and rising edge detection signals:
   signal startStopReg  : std_logic;
   signal lapReg        : std_logic;
   signal setToZeroReg  : std_logic;
   signal startStopRise : std_logic;
   signal lapRise       : std_logic;
   signal setToZeroRise : std_logic;

   -- Internal FSM output: state implies stopwatch's time must run:
   signal enableCounting : std_logic;

begin

   -- Detect when each control signal gets on:
   process (Clk, Reset)
   begin
      if Reset = '1' then
         startStopReg <= '0';
         lapReg       <= '0';
         setToZeroReg <= '0';
      elsif rising_edge (Clk) then
         startStopReg <= StartStop;
         lapReg       <= Lap;
         setToZeroReg <= SetToZero;
      end if;
   end process;

   startStopRise <= StartStop and not startStopReg;
   lapRise       <= Lap       and not lapReg;
   setToZeroRise <= SetToZero and not setToZeroReg;

   -- FSM, combinational section (next state):
  
   -- <<<< SUSTITUIR ESTE BLOQUE DE COMENTARIO POR:
   --      ESCRIBIR UN PROCESO COMBINACIONAL QUE ASIGNE nextState.
   --      TRUCOS PARA ESCRIBIR MENOS (DESCRIPCION MAS COMPACTA):
   --      - POR DEFECTO SE PUEDE, AL PRINCIPIO DEL CODIGO, ASIGNAR 
   --        COMO SIGUIENTE ESTADO EL ESTADO ACTUAL, 
   --        SOBRESCRIBIENDOLO EN LAS SIGUIENTES LINEAS DE CODIGO
   --        SOLO CUANDO SEA NECESARIO.
   --      - SI UNA CONDICION HACE QUE SE SALTE A UN ESTADO DESDE
   --        TODOS LOS DEMAS, SE PUEDE CODIFICAR EN UN "IF" PREVIO
   --        AL "CASE" HABITUAL.
      
   process(state, startStopRise, lapRise, setToZeroRise)
   begin
      nextState <= state;
      if setToZeroRise = '1' then
         nextState <= IDLE;
      else
         case state is
            when IDLE    => if startStopRise = '1' then nextState <= RUNNING; end if;
            when RUNNING => if startStopRise = '1' then nextState <= STOPPED;
                            elsif lapRise = '1' then nextState <= SHOWLAP; end if;
            when SHOWLAP => if startStopRise = '1' then nextState <= STOPPED;
                            elsif lapRise = '1' then nextState <= RUNNING; end if;
            when STOPPED => if startStopRise = '1' then nextState <= RUNNING; end if;
            when others  => nextState <= IDLE;
         end case;
       end if;
   end process;

   -- FSM, register for state:
  
   -- <<<< SUSTITUIR ESTE BLOQUE DE COMENTARIO POR:
   --      ESCRIBIR UN PROCESO SECUENCIAL QUE ASIGNE state A PARTIR
   --      DE nextState.
   --      RECORDATORIO: TOMAR COMO REFERENCIA LA DESCRIPCION DE UN FF 
   --      CON RESET ASINCRONO.
   
   process (Clk, Reset)
   begin
      if Reset = '1' then
         state <= IDLE;
      elsif rising_edge(Clk) then
         state <= nextState;
      end if;
   end process;
   
   -- Generate the outputs:
   enableCounting <= '1' when ((state = RUNNING) or (state = SHOWLAP)) else '0';
   UpdateDisplay  <= '0' when ((state = SHOWLAP) or (state = STOPPED)) else '1';

   EnableCountOut <= EnableCountIn and enableCounting;

   ClearCount <= setToZeroRise;

end Behavioral;

