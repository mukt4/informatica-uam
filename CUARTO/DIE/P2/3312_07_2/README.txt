Autores:
	Francisco Alcudia y Tomas Higuera Viso

Consideraciones:
	A pesar de entregarlo completo, no hemos conseguido hacer funcionar correctamente el modulo display.
	Sin embargo, si utilizamos el modulo display proporcionado, sí funciona correctamente

Archivos entregados:
	P2_stopwatch/
	├── netlist
	│   ├── display_ctrl.dcp
	│   ├── display_ctrl.edn
	│   └── display_ctrl.vhd
	├── README.txt
	├── rtl
	│   ├── counter.vhd
	│   ├── debounce.vhd
	│   ├── debug.log
	│   ├── display_ctrl.vhd
	│   ├── freq_divider.vhd
	│   ├── rst_adapt.vhd
	│   ├── stopwatch_fsm.vhd
	│   ├── stopwatch_pkg.vhd
	│   └── stopwatch.vhd
	├── sim
	│   ├── runsim.do
	│   ├── runsim_netlist.do
	│   ├── stopwatch_tb.vhd
	│   ├── vsim.wlf
	│   ├── wave.do
	│   └── work
	│       ├── _info
	│       ├── _lib1_0.qdb
	│       ├── _lib1_0.qpg
	│       ├── _lib1_0.qtl
	│       ├── _lib.qdb
	│       ├── @_opt
	│       │   ├── _lib1_0.qdb
	│       │   ├── _lib1_0.qpg
	│       │   ├── _lib1_0.qtl
	│       │   ├── _lib2_0.qdb
	│       │   ├── _lib2_0.qpg
	│       │   ├── _lib2_0.qtl
	│       │   ├── _lib3_0.qdb
	│       │   ├── _lib3_0.qpg
	│       │   ├── _lib3_0.qtl
	│       │   ├── _lib4_0.qdb
	│       │   ├── _lib4_0.qpg
	│       │   ├── _lib4_0.qtl
	│       │   └── _lib.qdb
	│       └── _vmake
	└── vivado
		├── P2_stopwatch
		│   ├── P2_stopwatch.cache
		│   │   └── wt
		│   │       ├── gui_handlers.wdf
		│   │       ├── java_command_handlers.wdf
		│   │       ├── project.wpc
		│   │       ├── synthesis_details.wdf
		│   │       ├── synthesis.wdf
		│   │       ├── webtalk_pa.xml
		│   │       └── xsim.wdf
		│   ├── P2_stopwatch.hw
		│   │   ├── hw_1
		│   │   │   └── hw.xml
		│   │   ├── P2_stopwatch.lpr
		│   │   └── webtalk
		│   │       ├── labtool_webtalk.log
		│   │       ├── usage_statistics_ext_labtool.html
		│   │       └── usage_statistics_ext_labtool.xml
		│   ├── P2_stopwatch.ip_user_files
		│   │   └── README.txt
		│   ├── P2_stopwatch.runs
		│   │   ├── impl_1
		│   │   │   ├── gen_run.xml
		│   │   │   ├── htr.txt
		│   │   │   ├── init_design.pb
		│   │   │   ├── ISEWrap.js
		│   │   │   ├── ISEWrap.sh
		│   │   │   ├── opt_design.pb
		│   │   │   ├── place_design.pb
		│   │   │   ├── project.wdf
		│   │   │   ├── route_design.pb
		│   │   │   ├── rundef.js
		│   │   │   ├── runme.bat
		│   │   │   ├── runme.log
		│   │   │   ├── runme.sh
		│   │   │   ├── stopwatch_14656.backup.vdi
		│   │   │   ├── stopwatch_8741.backup.vdi
		│   │   │   ├── stopwatch_bus_skew_routed.pb
		│   │   │   ├── stopwatch_bus_skew_routed.rpt
		│   │   │   ├── stopwatch_bus_skew_routed.rpx
		│   │   │   ├── stopwatch_clock_utilization_routed.rpt
		│   │   │   ├── stopwatch_control_sets_placed.rpt
		│   │   │   ├── stopwatch_drc_opted.pb
		│   │   │   ├── stopwatch_drc_opted.rpt
		│   │   │   ├── stopwatch_drc_opted.rpx
		│   │   │   ├── stopwatch_drc_routed.pb
		│   │   │   ├── stopwatch_drc_routed.rpt
		│   │   │   ├── stopwatch_drc_routed.rpx
		│   │   │   ├── stopwatch_io_placed.rpt
		│   │   │   ├── stopwatch_methodology_drc_routed.pb
		│   │   │   ├── stopwatch_methodology_drc_routed.rpt
		│   │   │   ├── stopwatch_methodology_drc_routed.rpx
		│   │   │   ├── stopwatch_opt.dcp
		│   │   │   ├── stopwatch_placed.dcp
		│   │   │   ├── stopwatch_power_routed.rpt
		│   │   │   ├── stopwatch_power_routed.rpx
		│   │   │   ├── stopwatch_power_summary_routed.pb
		│   │   │   ├── stopwatch_routed.dcp
		│   │   │   ├── stopwatch_route_status.pb
		│   │   │   ├── stopwatch_route_status.rpt
		│   │   │   ├── stopwatch.tcl
		│   │   │   ├── stopwatch_timing_summary_routed.pb
		│   │   │   ├── stopwatch_timing_summary_routed.rpt
		│   │   │   ├── stopwatch_timing_summary_routed.rpx
		│   │   │   ├── stopwatch_utilization_placed.pb
		│   │   │   ├── stopwatch_utilization_placed.rpt
		│   │   │   ├── stopwatch.vdi
		│   │   │   ├── vivado_14656.backup.jou
		│   │   │   ├── vivado_8741.backup.jou
		│   │   │   ├── vivado.jou
		│   │   │   └── vivado.pb
		│   │   └── synth_1
		│   │       ├── gen_run.xml
		│   │       ├── htr.txt
		│   │       ├── ISEWrap.js
		│   │       ├── ISEWrap.sh
		│   │       ├── rundef.js
		│   │       ├── runme.bat
		│   │       ├── runme.log
		│   │       ├── runme.sh
		│   │       ├── stopwatch.dcp
		│   │       ├── stopwatch.tcl
		│   │       ├── stopwatch_utilization_synth.pb
		│   │       ├── stopwatch_utilization_synth.rpt
		│   │       ├── stopwatch.vds
		│   │       ├── __synthesis_is_complete__
		│   │       ├── vivado.jou
		│   │       └── vivado.pb
		│   ├── P2_stopwatch.sim
		│   │   └── sim_1
		│   │       └── behav
		│   │           └── xsim
		│   │               ├── compile.log
		│   │               ├── compile.sh
		│   │               ├── elaborate.log
		│   │               ├── elaborate.sh
		│   │               ├── simulate.log
		│   │               ├── simulate.sh
		│   │               ├── stopwatch_tb_behav.wdb
		│   │               ├── stopwatch_tb.tcl
		│   │               ├── stopwatch_tb_vhdl.prj
		│   │               ├── webtalk_25761.backup.jou
		│   │               ├── webtalk_25761.backup.log
		│   │               ├── webtalk.jou
		│   │               ├── webtalk.log
		│   │               ├── xelab.pb
		│   │               ├── xsim.dir
		│   │               │   ├── stopwatch_tb_behav
		│   │               │   │   ├── Compile_Options.txt
		│   │               │   │   ├── obj
		│   │               │   │   │   ├── xsim_0.lnx64.o
		│   │               │   │   │   ├── xsim_1.c
		│   │               │   │   │   └── xsim_1.lnx64.o
		│   │               │   │   ├── TempBreakPointFile.txt
		│   │               │   │   ├── webtalk
		│   │               │   │   │   ├── usage_statistics_ext_xsim.html
		│   │               │   │   │   ├── usage_statistics_ext_xsim.wdm
		│   │               │   │   │   ├── usage_statistics_ext_xsim.xml
		│   │               │   │   │   └── xsim_webtalk.tcl
		│   │               │   │   ├── xsimcrash.log
		│   │               │   │   ├── xsim.dbg
		│   │               │   │   ├── xsimk
		│   │               │   │   ├── xsimkernel.log
		│   │               │   │   ├── xsim.mem
		│   │               │   │   ├── xsim.reloc
		│   │               │   │   ├── xsim.rlx
		│   │               │   │   ├── xsim.rtti
		│   │               │   │   ├── xsimSettings.ini
		│   │               │   │   ├── xsim.svtype
		│   │               │   │   ├── xsim.type
		│   │               │   │   └── xsim.xdbg
		│   │               │   └── xil_defaultlib
		│   │               │       ├── counter.vdb
		│   │               │       ├── debounce.vdb
		│   │               │       ├── freq_divider.vdb
		│   │               │       ├── rst_adapt.vdb
		│   │               │       ├── stopwatch_fsm.vdb
		│   │               │       ├── stopwatch_pkg.vdb
		│   │               │       ├── stopwatch_tb.vdb
		│   │               │       ├── stopwatch.vdb
		│   │               │       └── xil_defaultlib.rlx
		│   │               ├── xsim.ini
		│   │               ├── xvhdl.log
		│   │               └── xvhdl.pb
		│   ├── P2_stopwatch.xpr
		│   └── stopwatch_tb_behav.wcfg
		└── stopwatch.xdc

