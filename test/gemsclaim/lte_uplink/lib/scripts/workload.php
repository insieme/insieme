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

$first = 1;
$sum = 0;
$items = 0;
$time = 3;

if ($argc == 2) {
  $raw = fopen($argv[1], "r");
} else {
  echo "You need to give the file to parse as argument\n";
  exit();
}

if ($raw) {
  $file =  fopen($argv[1] ."_processed",  "w");
  while (($line=fgets($raw)) !== false) {
    if (preg_match("/Second/", $line, $match)) {
      if ($first==0) {
	fprintf($file, "%3.2f\t%2.2f\n", $time, ($sum/$items)/100);
      }
      $first = 0;
      $items = 0;
      $sum   = 0;
      $time += $DELTA;
    } else {
      $items++;
      $sum += $line;
    }
  }
  fprintf($file, "%3.2f\t%2.2f\n", $time, ($sum/$items)/4);
}
?>