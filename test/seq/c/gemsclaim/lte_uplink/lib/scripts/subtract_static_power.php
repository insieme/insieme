#!/usr/bin/php
<?php
/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

$rms    = fopen("run/RMS_samples_nap+usleep_avg19.9A", "r");
$static = fopen("run/static_savings", "r");

$match2[1] = 0;
$match2[2] = 0;

$power = 0;
$items = 0;

if ($rms) {
  while (($line=fgets($rms)) !== false) {
    preg_match("/([0-9.]*)\t([0-9.]*)/", $line, $match);

    if ($match[1] < $match2[1]) {
      echo $match[1] ."\t" .($match[2]-$match2[2]) ."\n";
      $power += $match[2]-$match2[2];
      $items++;
      continue;
    }

    while ($match[1] > $match2[1]) {
      if (($line2 = fgets($static)) !== false)
	preg_match("/([0-9.]*)\t([-0-9.]*)/", $line2, $match2);
      else
	break;
    }
    echo $match[1] ."\t" .($match[2]-$match2[2]) ."\n";
    $power += $match[2]-$match2[2];
    $items++;
  }
}
echo "Average RMS current: " .$power/$items;
?>
