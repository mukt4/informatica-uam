# ZYBO xdc
# define clock and period
create_clock -period 8.000 -name clk_pin -waveform {0.000 4.000} [get_ports clk_pin]

# input delay
set_input_delay -clock clk_pin 0 [get_ports rxd_pin]
set_input_delay -clock clk_pin -min -0.5 [get_ports rxd_pin]

set_input_delay -clock clk_pin -max 0.0 [get_ports btn_pin]
set_input_delay -clock clk_pin -min -0.5 [get_ports btn_pin]

#output delay
set_output_delay -clock clk_pin 0 [get_ports led_pins[*]]


