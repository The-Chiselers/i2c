#!/bin/sh

# Example Usage: synth.sh GPIO

# The Following Vars are set by the development flake:
# - BUILD_ROOT
# - PROJECT_ROOT
# - TOP

# Exit on error
set -e

# if ENV vars are not set, error out
if [ -z ${BUILD_ROOT} ]; then
    echo "BUILD_ROOT is not set. Exiting..."
    exit 1
fi
if [ -z ${PROJECT_ROOT} ]; then
    echo "PROJECT_ROOT is not set. Exiting..."
    exit 1
fi
if [ -z ${TOP} ]; then
    echo "TOP is not set. Exiting..."
    exit 1
fi

# Set up build directories
if [ ! -e ${BUILD_ROOT}/synth ]; then 
    mkdir -p ${BUILD_ROOT}/synth
fi

# Removing old synth.tcl
if [ -e ${BUILD_ROOT}/synth/synth.tcl ]; then 
    rm -f ${BUILD_ROOT}/synth/synth.tcl
fi

# Setting up synth.tcl
echo "set top ${TOP}" >> ${BUILD_ROOT}/synth/synth.tcl
echo "set techLib ${PROJECT_ROOT}/synth/stdcells.lib" >> ${BUILD_ROOT}/synth/synth.tcl
echo "yosys -import" >> ${BUILD_ROOT}/synth/synth.tcl
echo "set f [open ${BUILD_ROOT}/verilog/filelist.f]" >> ${BUILD_ROOT}/synth/synth.tcl
echo "while {[gets \$f line] > -1} {" >> ${BUILD_ROOT}/synth/synth.tcl
echo "  read_verilog -sv ${BUILD_ROOT}/verilog/\$line" >> ${BUILD_ROOT}/synth/synth.tcl
echo "}" >> ${BUILD_ROOT}/synth/synth.tcl
echo "close \$f" >> ${BUILD_ROOT}/synth/synth.tcl
echo "hierarchy -check -top \$top" >> ${BUILD_ROOT}/synth/synth.tcl
echo "synth -top \$top" >> ${BUILD_ROOT}/synth/synth.tcl
echo "flatten" >> ${BUILD_ROOT}/synth/synth.tcl
echo "dfflibmap -liberty \$techLib" >> ${BUILD_ROOT}/synth/synth.tcl
echo "abc -liberty \$techLib" >> ${BUILD_ROOT}/synth/synth.tcl
echo "opt_clean -purge" >> ${BUILD_ROOT}/synth/synth.tcl
echo "write_verilog -noattr \$top\_net.v" >> ${BUILD_ROOT}/synth/synth.tcl
echo "stat -liberty \$techLib" >> ${BUILD_ROOT}/synth/synth.tcl


# Running Synthesis
cd ${BUILD_ROOT}/synth
mkdir -p ${BUILD_ROOT}/synth/
yosys -Qv 1 ${BUILD_ROOT}/synth/synth.tcl -p "tee -o ${BUILD_ROOT}/synth/synth.rpt stat"
