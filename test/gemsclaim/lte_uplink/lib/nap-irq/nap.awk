################################################################################
#                       LTE UPLINK RECEIVER PHY BENCHMARK                      #
#                                                                              #
# This file is distributed under the license terms given by LICENSE.TXT        #
################################################################################
# Author: Magnus Sjalander                                                     #
################################################################################

// {
    increment=0;
    if (match($1, /.*([1-7])([0-9a-f][0-9a-f][0-9a-f]):/, tmp)) {
	print "s/HMS_NAP_LSB/0x"tmp[1]tmp[2]"/";
    } else if (match($1, /.*([8-9a-f])([0-9a-f][0-9a-f][0-9a-f]):/, tmp)) {
	print "s/HMS_NAP_LSB_HMS/0xffff"tmp[1]tmp[2]"/";
	increment=1;
    }
    if (match($1, /(.*)[0-9a-f][0-9a-f][0-9a-f][0-9a-f]:/, tmp)) {
	print "s/HMS_NAP_MSB/0x"(tmp[1] + increment)"/";
    }
}