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
        yrange: 12 26

#proc xaxis
        label: Time (s)
        labeldetails: size=14 adjust=0.0,-0.15
        stubdetails: align=C size=12 style=R
        tics: yes
        stubs: inc 20

#proc yaxis
    label: Power (W)
        labeldetails: size=14 adjust=-0.4,0.0
        stubs: inc 1
        stubdetails: align=R size=12 style=R
        grid: color=gray(0.7)

#proc getdata
        pathname: log/RMS_samples_nonap_avg25A

#proc lineplot
        xfield 1
        yfield 2
        legendlabel: NONAP
        linedetails: color=yelloworange style=0 dashscale=5.0
legendsampletype: line+symbol

#proc getdata
        pathname: log/RMS_samples_usleep_avg20.7A

#proc lineplot
        xfield 1
        yfield 2
        legendlabel: IDLE
        linedetails: color=red style=0 dashscale=5.0
legendsampletype: line+symbol

#proc getdata
        pathname: log/RMS_samples_nap+usleep_avg19.9A

#proc lineplot
        xfield 1
        yfield 2
        legendlabel: NAP+IDLE
        linedetails: color=green style=0 dashscale=5.0
legendsampletype: line+symbol

#proc getdata
        pathname: log/RMS_samples_static_avg18.5A

#proc lineplot
        xfield 1
        yfield 2
        legendlabel: PowerGating
        linedetails: color=purple style=0 dashscale=5.0
legendsampletype: line+symbol

#proc areadef
        rectangle: 1 0.7 10.1 8.3
        xrange: 0 340
        yrange: 0 1

#proc xaxis
        label: Time (s)
        labeldetails: size=14 adjust=0.0,-0.15
        stubdetails: align=C size=12 style=R
        tics: yes
        stubs: inc 20

#proc yaxis
    label: Activity
        labeldetails: size=14 adjust=1.2,0.0
        stubs: inc 0.071428
        location: 10.1
        stubdetails: adjust=0.6,0 align=R
	stubformat %1.2f
        grid: color=gray(0.7)

#proc getdata
        pathname: log/workload_nap_nousleep_processed

#proc lineplot
        xfield 1
        yfield 2
        legendlabel: Activity
        linedetails: color=black style=0 dashscale=10.0
legendsampletype: line+symbol

#proc legend
  location min+1.0 max
  textdetails: size=16
  format: multiline
  seglen: 0.8
