# i2c

## Dependancies

- Nix Package Manager
    - Windows: Recommend Using the [NixOS WSL](https://nix-community.github.io/NixOS-WSL/install.html) image
    - MacOS: [MacOS Nix Package Manager Setup](https://nixos.org/download/#nix-install-macos)
    - Other Linux Distributions: [Linux Nix Package Manager Setup](https://nixos.org/download/#nix-install-linux)

## Setup

```bash
git clone [url]
cd [folder]
# This step can take a long time
# This is due to building system-c and verilator from source with clang
# It should only take a long time the first time
sh dev_shell.sh 

```

## Usage

**Note:** All build artifacts will be generated in the "out" folder

Generate Verilog: `make verilog`

Run Tests: `make test`

Generate Coverage: `make cov`

Run Synthesis: `make synth`

Run STA: `make sta`