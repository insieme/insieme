# setup environment variables
. environment.setup

echo "Installing all libraries into $PREFIX using $SLOTS cores ..."

for cur in *_install.sh ; do 
	echo "  Running $cur ..."
	#. $cur
done

echo "Installation complete!"

