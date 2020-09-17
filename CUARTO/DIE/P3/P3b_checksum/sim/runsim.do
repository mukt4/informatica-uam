# --------------------------------------------------------
# ModelSim/Questa simulation script.
# --------------------------------------------------------

# Create library work (deleting it before if it already exists):
if [file exists work] {
   vdel -lib work -all
}
vlib work

# Compile RTL files:
vcom -explicit -check_synthesis -93 "../rtl/chksum.vhd"

# Compile testbench:
vcom -explicit  -93 "tb_fifo_pkg.vhd"
vcom -explicit  -93 "chksum_tb.vhd"

# Invoke the simulator. Resolution 1 ps, avoid nodes disappearing due to optimization:
vsim -t 1ps -voptargs="+acc" -lib work chksum_tb

# Record information about all signals:
log -r /*

# Load waveforms:
do wave.do

# Use a simulator option to avoid warnings on initial X in datapath:
set StdArithNoWarnings 1
run 50 ns
set StdArithNoWarnings 0

# Run full simulation:
run -all


