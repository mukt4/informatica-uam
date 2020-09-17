# --------------------------------------------------------
# ModelSim/Questa simulation script for project stopwatch.
#  VERSION FOR USING netlist/display_ctrl.vhd
# --------------------------------------------------------

# Create library work (deleting it before if it already exists):
if [file exists work] {
   vdel -lib work -all
}
vlib work

# Compile netlist files:
vcom -explicit -93 "../netlist/display_ctrl.vhd"

# Compile RTL files:
vcom -explicit -check_synthesis -93 "../rtl/stopwatch_pkg.vhd"
vcom -explicit -check_synthesis -93 "../rtl/counter.vhd"
vcom -explicit -check_synthesis -93 "../rtl/debounce.vhd"
vcom -explicit -check_synthesis -93 "../rtl/freq_divider.vhd"
vcom -explicit -check_synthesis -93 "../rtl/rst_adapt.vhd"
vcom -explicit -check_synthesis -93 "../rtl/stopwatch_fsm.vhd"
vcom -explicit -check_synthesis -93 "../rtl/stopwatch.vhd"

# Compile testbench:
vcom -explicit  -93 "stopwatch_tb.vhd"

# Invoke the simulator. Resolution 1 ps, avoid nodes disappearing due to optimization:
vsim -t 1ps -voptargs="+acc" -lib work stopwatch_tb

# Record information about all signals:
log -r /*

# Load waveforms:
do wave.do

# Use a simulator option to avoid typical warnings in time=0:
set StdArithNoWarnings 1
run 0 ns
set StdArithNoWarnings 0

# Run full simulation:
run -all


