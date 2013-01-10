#!/bin/bash

if [ `whoami` != root ]; then
	echo "Please run this script as root or using sudo"
	exit
fi

echo -n "Loading msr module..."
modprobe msr
if [ $? -ne 0 ]; then
	echo "failed!"
	exit
fi
echo "done!"

echo -n "Changing permission of msr files..."
chmod 666 /dev/cpu/*/msr
if [ $? -ne 0 ]; then
	echo "failed!"
	exit
fi
echo "done!"

echo -n "Changing permissions of cpufreq files..."
chmod 666 /sys/devices/system/cpu/cpu*/cpufreq/scaling_max_freq
if [ $? -ne 0 ]; then
	echo "failed!"
	exit
fi
echo "done!"
