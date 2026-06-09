#!/usr/bin/env bash
set -euo pipefail

FORMAT=${1:-vhdl}   # vhdl | verilog | systemverilog

exec stack run clash -- Example.Project "--$FORMAT"
