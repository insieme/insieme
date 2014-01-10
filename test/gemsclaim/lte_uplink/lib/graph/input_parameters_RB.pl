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
        xrange: 0 67800
        yrange: 0 200

#proc xaxis
        label: Subframe
        labeldetails: size=14 adjust=0.0,-0.15
        stubdetails: align=C size=12 style=R
        tics: yes
        stubs: inc 5000

#proc yaxis
    label: Physical Resource Blocks
        labeldetails: size=14 adjust=-0.4,0.0
        stubs: inc 10
        stubdetails: align=R size=12 style=R
        grid: color=gray(0.7)

#proc getdata
        pathname: log/input_parameters_processed

#proc lineplot
        xfield 1
        yfield 4
        legendlabel: Max
        linedetails: color=darkblue style=0 dashscale=5.0
legendsampletype: line+symbol

#proc lineplot
        xfield 1
        yfield 3
        legendlabel: Total
        linedetails: color=red style=0 dashscale=5.0
legendsampletype: line+symbol

#proc lineplot
        xfield 1
        yfield 5
        legendlabel: Min
        linedetails: color=green style=0 dashscale=5.0
legendsampletype: line+symbol

#proc legend
  location: min+0.55 min-0.35
  textdetails: size=16
  format: singleline
 specifyorder: 
  Total 
  Max 
  Min
