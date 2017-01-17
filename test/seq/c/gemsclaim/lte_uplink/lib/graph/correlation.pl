////////////////////////////////////////////////////////////////////////////////
//                      LTE UPLINK RECEIVER PHY BENCHMARK                     //
//                                                                            //
// This file is distributed under the license terms given by LICENSE.TXT      //
////////////////////////////////////////////////////////////////////////////////
// Author: Magnus Sjalander                                                   //
////////////////////////////////////////////////////////////////////////////////

#proc page
 landscape: yes
#proc areadef
 rectangle: 1 0.7 10.8 8.3
 xrange: 0 200
 yrange: 0 100

#proc xaxis
 label: Physical Resource Blocks
 labeldetails: size=14 adjust=0.0,-0.15
 stubdetails: align=C size=12 style=R
 tics: yes
 stubs: inc 10

#proc yaxis
 label: Activity (%)
 labeldetails: size=14 adjust=-0.4,0.0
 stubs: inc 5
 stubdetails: align=R size=12 style=R
 grid: color=gray(0.7)

////////////////////////////////////////////////////////////
// QPSK
////////////////////////////////////////////////////////////

#proc getdata
 pathname: log/single_user_correlation_L1_QPSK_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: QPSK-1-layer
 linedetails: color=blue style=1 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L2_QPSK_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: QPSK-2-layers
 linedetails: color=kelleygreen style=1 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L3_QPSK_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: QPSK-3-layers
 linedetails: color=yellowgreen style=1 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L4_QPSK_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: QPSK-4-layers
 linedetails: color=red style=1 dashscale=10.0 width=1
 legendsampletype: line+symbol

////////////////////////////////////////////////////////////
// 16QAM
////////////////////////////////////////////////////////////

#proc getdata
 pathname: log/single_user_correlation_L1_16QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 16QAM-1-layer
 linedetails: color=powderblue style=8 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L2_16QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 16QAM-2-layers
 linedetails: color=teal style=8 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L3_16QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 16QAM-3-layers
 linedetails: color=redorange style=8 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L4_16QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 16QAM-4-layers
 linedetails: color=claret style=8 dashscale=10.0 width=1
 legendsampletype: line+symbol

////////////////////////////////////////////////////////////
// 64QAM
////////////////////////////////////////////////////////////

#proc getdata
 pathname: log/single_user_correlation_L1_64QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 64QAM-1-layer
 linedetails: color=purple style=0 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L2_64QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 64QAM-2-layers
 linedetails: color=brightgreen style=0 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L3_64QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 64QAM-3-layers
 linedetails: color=yelloworange style=0 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc getdata
 pathname: log/single_user_correlation_L4_64QAM_err

# #proc bars
#  lenfield: 2
#  locfield: 1
#  thinbarline: color=black width=1.0
#  segmentfields: 3 4
#  tails: 0.02
#  truncate: yes

#proc lineplot
 xfield: 1
 yfield: 2
 legendlabel: 64QAM-4-layers
 linedetails: color=magenta style=0 dashscale=10.0 width=1
 legendsampletype: line+symbol

#proc legend
 location: min+2.0 max
 textdetails: size=16
 format: multiline
 seglen: 1.5
