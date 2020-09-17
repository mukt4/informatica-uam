onerror {resume}
quietly WaveActivateNextPane {} 0
add wave -noupdate /chksum_tb/Clk
add wave -noupdate /chksum_tb/PktData0
add wave -noupdate /chksum_tb/PktData1
add wave -noupdate /chksum_tb/PktData2
add wave -noupdate /chksum_tb/PktData3
add wave -noupdate /chksum_tb/PktData4
add wave -noupdate /chksum_tb/PktData5
add wave -noupdate /chksum_tb/PktData6
add wave -noupdate /chksum_tb/PktData7
add wave -noupdate /chksum_tb/SoP
add wave -noupdate /chksum_tb/ChksumValid
add wave -noupdate /chksum_tb/ChksumOut
add wave -noupdate /chksum_tb/Chksum_Checked
add wave -noupdate /chksum_tb/Chksum_Gold
add wave -noupdate /chksum_tb/endsim
TreeUpdate [SetDefaultTree]
WaveRestoreCursors {{Cursor 1} {97 ps} 0}
quietly wave cursor active 1
configure wave -namecolwidth 290
configure wave -valuecolwidth 100
configure wave -justifyvalue left
configure wave -signalnamewidth 0
configure wave -snapdistance 10
configure wave -datasetprefix 0
configure wave -rowmargin 4
configure wave -childrowmargin 2
configure wave -gridoffset 0
configure wave -gridperiod 1
configure wave -griddelta 40
configure wave -timeline 0
configure wave -timelineunits ps
update
WaveRestoreZoom {0 ps} {869 ps}
