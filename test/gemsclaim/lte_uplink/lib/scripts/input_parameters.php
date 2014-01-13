#!/usr/bin/php
<?php
/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

$DELTA = 1;

$time = 0;

if ($argc == 2) {
  $raw = fopen($argv[1], "r");
} else {
  echo "You need to give the file to parse as argument\n";
  exit();
}

if ($raw) {
  $file =  fopen($argv[1] ."_processed",  "w");
  while (($line=fgets($raw)) !== false) {
    $time += $DELTA;
    for ($i=0; $i<24; $i++) {
      $line=fgets($raw);
      $time += $DELTA;
    }
    $items = preg_split("/[\s,]+/", trim($line));
    $users = (int)$items[0];
    $totRB = (int)$items[1]*2;
    $maxRB = (int)$items[2]*2;
    $minRB = (int)$items[3]*2;
    fprintf($file, "%6.3f %2s %3s %3s %3s %s %s %s %s %s\n", $time, $users, $totRB, $maxRB, $minRB, $items[4], $items[5], $items[6], $items[7], $items[8]);
  }
}
?>
