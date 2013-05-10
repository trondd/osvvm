##########################################################################
# (c) Aldec, Inc.
# All rights reserved.
# SENSORS example with Randomization and Coverage Packages
# Date Modified: Jun-22-2012
#
# This script will compile and simulate the SENSORS design in Riviera-PRO.
# Make sure to change the directory to YourPath>/example/Sensors
###########################################################################

alib work

#compiling the randomization and coverage packages
acom -2008 -incr \
	../../packages/SortListPkg_int.vhd \
	../../packages/RandomBasePkg.vhd \
	../../packages/RandomPkg.vhd \
	../../packages/CoveragePkg.vhd

#compiling the main design files
acom -dbg -2008 ./src/sensors.vhd

#starting the simulation
#asim -dbg +access +r -GIntelligent=False -GDataSDinc=0.0 sensors 
asim -dbg +access +r -GIntelligent=True -GDataSDinc=4.0 sensors 
#asim -dbg +access +r -GIntelligent=True -GDataSDinc=8.0 sensors 

#logging all the signals
log -rec /*

run -all
