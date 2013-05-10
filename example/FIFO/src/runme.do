##########################################################################
# (c) Aldec, Inc.
# All rights reserved.
# FIFO example with Randomization and Coverage Packages
# Date Modified: Dec-20-2011
#
# This script will compile and simulate the FIFO design in Riviera-PRO.
# Make sure to change the directory to YourPath>/example/FIFO 
###########################################################################

alib work

#compiling the randomization and coverage packages
acom -2008 -incr \
	../../packages/SortListPkg_int.vhd \
	../../packages/RandomBasePkg.vhd \
	../../packages/RandomPkg.vhd \
	../../packages/CoveragePkg.vhd

#compiling the main design files
acom ./src/fifo.vhd
acom -2008 -dbg ./src/tb_top.vhd

#starting the simulation
asim +access +r tb_top 
#logging all the signals
log -rec /*
#adding DUT signals to the waveform
add wave /tb_top/DUT/*

run -all
