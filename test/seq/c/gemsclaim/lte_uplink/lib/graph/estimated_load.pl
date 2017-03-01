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
        rectangle: 1 0.7 10.1 8.3
        xrange: 0 340
        yrange: 0 1.0

#proc xaxis
        label: Time (s)
        labeldetails: size=14 adjust=0.0,-0.15
        stubdetails: align=C size=12 style=R
        tics: yes
        stubs: inc 20

#proc yaxis
    label: Activity
        labeldetails: size=14 adjust=-0.4,0.0
        stubs: inc .1
        stubdetails: align=R size=12 style=R
        grid: color=gray(0.7)

#proc getdata
        pathname: log/estimated_load_processed

#proc lineplot
        xfield 1
        yfield 2
        legendlabel: Estimated
        linedetails: color=darkblue style=0 dashscale=5.0
legendsampletype: line+symbol

#proc lineplot
        xfield 1
        yfield 3
        legendlabel: Measured
        linedetails: color=yelloworange style=0 dashscale=5.0
legendsampletype: line+symbol


#proc legend
  location min+2.0 max
  textdetails: size=16
  format: multiline
  seglen: 1.5
