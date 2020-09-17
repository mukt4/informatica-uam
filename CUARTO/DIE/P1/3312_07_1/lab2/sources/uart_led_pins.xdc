# ZYBO xdc
# LED [3:0]
############################
# On-board led             #
############################
set_property PACKAGE_PIN M14 [get_ports led_pins[0]]
set_property IOSTANDARD LVCMOS33 [get_ports led_pins[0]]
set_property PACKAGE_PIN M15 [get_ports led_pins[1]]
set_property IOSTANDARD LVCMOS33 [get_ports led_pins[1]]
set_property PACKAGE_PIN G14 [get_ports led_pins[2]]
set_property IOSTANDARD LVCMOS33 [get_ports led_pins[2]]
set_property PACKAGE_PIN D18 [get_ports led_pins[3]]
set_property IOSTANDARD LVCMOS33 [get_ports led_pins[3]]

# CLK source 125 MHz
set_property PACKAGE_PIN L16 [get_ports clk_pin]
set_property IOSTANDARD LVCMOS33 [get_ports clk_pin]

# BTN0
set_property PACKAGE_PIN R18 [get_ports btn_pin]
set_property IOSTANDARD LVCMOS33 [get_ports btn_pin]

# RXD UART using PmodUSBUART Pin 3 on JE PMOD 
set_property PACKAGE_PIN J15 [get_ports rxd_pin]
set_property IOSTANDARD LVCMOS33 [get_ports rxd_pin]

# Reset - BTN1
set_property PACKAGE_PIN P16 [get_ports rst_pin]
set_property IOSTANDARD LVCMOS33 [get_ports rst_pin]

