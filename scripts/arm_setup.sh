# setup environment variables
. ./environment.setup

wget -nc https://github.com/thoughtpolice/enable_arm_pmu/archive/master.zip

RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

unzip master.zip
cd enable_arm_pmu-master/ko
make all
cd ../
./load-module

# Check for failure
RET=$?
if [ $RET -ne 0 ]; then
	exit $RET
fi

echo "#### Cleaning up environment ####"
cd ../
rm -R enable_arm_pmu-master
rm -R master.zip

exit 0
