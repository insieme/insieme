#!/bin/bash

if [ `whoami` != root ]; then
	echo Please run this script as root or using sudo
	exit
fi

modprobe msr
chmod 666 /dev/cpu/*/msr
chmod 666 /sys/devices/system/cpu/cpu*/cpufreq/scaling_max_freq
