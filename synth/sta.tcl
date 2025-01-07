
set top $::env(TOP)
set projectRoot $::env(PROJECT_ROOT)
set buildRoot $::env(BUILD_ROOT)

read_liberty $projectRoot/synth/stdcells.lib
read_verilog $buildRoot/synth/$top\_net.v
link_design $top
source $buildRoot/sta/${top}.sdc
check_setup
report_checks