----------------------------------------------------------------------------------
-- Company: EPS - UAM
-- Engineer: 
-- 
-- Create Date: 09/21/2018 08:53:21 AM
-- Design Name: uart_led
-- Module Name: uart_led2.vhdl - Behavioral
-- Project Name: 
-- Description: 
--     Ties the UART receiver to the LED controller
--  Parent   : None
--  Children : uart_rx.v led_ctl.v 
--
--  Description: 
--             VHDL version of uart_led.v
--
----------------------------------------------------------------------------------


library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

--use IEEE.NUMERIC_STD.ALL;
--library UNISIM;
--use UNISIM.VComponents.all;

entity uart_led2 is
    Port ( clk_pin : in STD_LOGIC;
           rst_pin : in STD_LOGIC;
           btn_pin : in STD_LOGIC;
           rxd_pin : in STD_LOGIC;
           led_pins : out STD_LOGIC_VECTOR (3 downto 0));
end uart_led2;

architecture Behavioral of uart_led2 is

--***************************************************************************
-- Parameter definitions
--***************************************************************************
  constant BAUD_RATE  : integer := 115_200;   
  constant CLOCK_RATE : integer := 125_000_000;

component meta_harden is
port (
  clk_dst : in STD_LOGIC;      -- Destination clock
  rst_dst: in STD_LOGIC;     -- Reset - synchronous to destination clock
  signal_src: in STD_LOGIC;   --Asynchronous signal to be synchronized
  signal_dst : out STD_LOGIC    -- Synchronized signal
); 
end component;

component uart_rx is
   generic(CLOCK_RATE: integer;
           BAUD_RATE:  integer );
   port (
      -- Input Ports - Single Bit
      clk_rx      :  in  std_logic;
      rst_clk_rx  :  in  std_logic;
      rxd_i       :  in  std_logic;
      frm_err     :  out  std_logic;
      rx_data_rdy :  out  std_logic;
      rxd_clk_rx  :  out  std_logic;
      rx_data     :  out  std_logic_vector(7 downto 0)
   );
end component uart_rx;

component led_ctl is
   port (
      btn_clk_rx  :  in  std_logic;
      clk_rx      :  in  std_logic;
      rst_clk_rx  :  in  std_logic;
      rx_data_rdy :  in  std_logic;
      rx_data     :  in  std_logic_vector(7 downto 0);
      led_o       :  out  std_logic_vector(3 downto 0)
   );
end component led_ctl;

signal rst_clk_rx: STD_LOGIC; 
signal btn_clk_rx: STD_LOGIC; 
signal rx_data_rdy: STD_LOGIC; 

signal rx_data: STD_LOGIC_VECTOR(7 downto 0);

begin

-- rst_pin input
meta_harden_rst_i0: meta_harden port map (
  clk_dst => clk_pin, 
  rst_dst => '1',
  signal_src => rst_pin,
  signal_dst  => rst_clk_rx ); 

-- the button input
meta_harden_btn_i0: meta_harden port map (
  clk_dst => clk_pin, 
  rst_dst => rst_clk_rx,
  signal_src => btn_pin,
  signal_dst  => btn_clk_rx ); 
  
  uart_rx_inst: uart_rx
     generic map ( 
        CLOCK_RATE => CLOCK_RATE,
        BAUD_RATE => BAUD_RATE)
     port map (
        clk_rx              => clk_pin,            
        rst_clk_rx          => rst_clk_rx,        
        rxd_i               => rxd_pin,
        rxd_clk_rx          => open,
        rx_data_rdy         => rx_data_rdy,       
        rx_data             => rx_data,
        frm_err             => open           
     );

led_ctl_inst: led_ctl
   port map (
      clk_rx              => clk_pin,
      rst_clk_rx          => rst_clk_rx,        
      btn_clk_rx          => btn_clk_rx,        
      rx_data             => rx_data,        
      rx_data_rdy         => rx_data_rdy,
      led_o(3 downto 0)   => led_pins
   );
  
end Behavioral;
