#-------------------------------------------------------------------------------
# Constraints for stopwatch in Zybo board
#-------------------------------------------------------------------------------
#

#-------------------------------------------------------------------------------
# 1) TIMING CONSTRAINTS:

# Clock 125 MHz:
create_clock -name Clk -period 8.00 -waveform {0 4} [get_ports {Clk}]

#-------------------------------------------------------------------------------
# 2) PIN LOCATION AND I/O STANDARD CONSTRAINTS:

# Clock input:
set_property -dict "PACKAGE_PIN L16  IOSTANDARD LVCMOS33" [get_ports Clk]

# Push-Buttons 0 to 3:
set_property -dict "PACKAGE_PIN R18  IOSTANDARD LVCMOS33" [get_ports Lap      ]
set_property -dict "PACKAGE_PIN P16  IOSTANDARD LVCMOS33" [get_ports StartStop]
set_property -dict "PACKAGE_PIN V16  IOSTANDARD LVCMOS33" [get_ports SetToZero]
set_property -dict "PACKAGE_PIN Y16  IOSTANDARD LVCMOS33" [get_ports ResetX   ]



# 7-Segments display pmod in upper ROW of JD connector:
set_property -dict "PACKAGE_PIN T15  IOSTANDARD LVCMOS33" [get_ports Ser ] ;# JD2
set_property -dict "PACKAGE_PIN P14  IOSTANDARD LVCMOS33" [get_ports SClk] ;# JD3
set_property -dict "PACKAGE_PIN R14  IOSTANDARD LVCMOS33" [get_ports RClk] ;# JD4

#-------------------------------------------------------------------------------
