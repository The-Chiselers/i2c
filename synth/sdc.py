import os
import sys
import re

import argparse

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument("--top", required=True, help="Top module name")
    parser.add_argument("--out", required=True, help="Output SDC file")

    # Optional and use as many as needed
    example_clock = """
    --clock clock=5.0 --clock clock2=10.0
"""
    parser.add_argument("--clock", required=True, help=f"Clocks to add to the SDC file, example: {example_clock}", action='append' )
    return parser.parse_args()


# This script will generate an SDC file for the given top module
# It will look at the netlist file and extract the ports
# It will then create an SDC file with the following:
# - create_clock for each clock
# - set_input_delay for each input
# - set_output_delay for each output
def main():
    if not "BUILD_ROOT" in os.environ:
        print("BUILD_ROOT not set, please set and rerun")
        sys.exit(1)
    
    args = parse_args()
    top = args.top
    out = args.out

    build_root = os.environ["BUILD_ROOT"]
    synth_build_root = os.path.join(build_root, "synth")

    clocks = args.clock

    netlist_path = os.path.join(synth_build_root, f"{top}_net.v")
    netlist = open(netlist_path, "r").read()

    
    # ports look like this:
    # module ${TOP}(.*?);
    ports = re.search(r"module\s+{}\s*\((.*?)\);".format(top), netlist, re.DOTALL).group(1).split(",")
    ports = [port.strip() for port in ports]
    
    ports_map = {}
    for port in ports:
        ports_map[port] = "NONE"

    # for each line if matches input or output, add to ports_map if port is in map
    for line in netlist.split("\n"):
        if "input" in line or "output" in line:
            port = re.search(r"(input|output)\s+(?:\[\d+:\d+\])?\s*(.*?);", line).group(2)
            if port in ports_map:
                ports_map[port] = re.search(r"(input|output)", line).group(1)
    
    # create sdc file
    sdc = open(out, "w")

    for clock in clocks:
        clock_name, period = clock.split("=")
        sdc.write("create_clock -period {} -waveform {{0 {}}} {}\n".format(period, float(period)/2, clock_name))

    clock_names = [clock.split("=")[0] for clock in clocks]

    inputs = []
    outputs = []
    for port, direction in ports_map.items():
        if direction == "input" and not port in clock_names:
            inputs.append(port)
        elif direction == "output" and not port in clock_names:
            outputs.append(port)

    for input_port in inputs:
        sdc.write("set_input_delay -clock " + clock_names[0] + " 1.0 {" + input_port + "}\n")

    for output_port in outputs:
        sdc.write("set_output_delay -clock " + clock_names[0] + " 1.0 {" + output_port + "}\n")



if __name__ == "__main__":
    main()