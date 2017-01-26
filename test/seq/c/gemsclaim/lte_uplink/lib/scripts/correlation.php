#!/usr/bin/php
<?php
/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

$first = 1;
$sum = 0;
$items = 0;
$min = 100;
$max = 0;

$RB;
$layer;
$mod;
$file;
$inc = 0;

if ($argc == 2) {
  $raw = fopen($argv[1], "r");
} else {
  echo "You need to give the file to parse as argument\n";
  exit();
}

if ($raw) {
  $file_array[1]  = fopen($argv[1] ."_L1_QPSK",  "w");
  $file_array[2]  = fopen($argv[1] ."_L2_QPSK",  "w");
  $file_array[3]  = fopen($argv[1] ."_L3_QPSK",  "w");
  $file_array[4]  = fopen($argv[1] ."_L4_QPSK",  "w");
  $file_array[5]  = fopen($argv[1] ."_L1_16QAM", "w");
  $file_array[6]  = fopen($argv[1] ."_L2_16QAM", "w");
  $file_array[7]  = fopen($argv[1] ."_L3_16QAM", "w");
  $file_array[8]  = fopen($argv[1] ."_L4_16QAM", "w");
  $file_array[9]  = fopen($argv[1] ."_L1_64QAM", "w");
  $file_array[10]  = fopen($argv[1] ."_L2_64QAM", "w");
  $file_array[11] = fopen($argv[1] ."_L3_64QAM", "w");
  $file_array[12] = fopen($argv[1] ."_L4_64QAM", "w");

  $previous_layer = -1;

  while (($line=fgets($raw)) !== false) {
    if (preg_match("/RB=[ ]*([0-9]*), Layer=[ ]*([1-4]*), Mod=([0,1,2]*)/", $line, $match)) {
      if ($first==0) {
	fprintf($file, "%2.2f %4s  %2.2f\n", ($sum/$items), $RB*2, $max-$min);
      }
      $RB    = $match[1];
      $layer = $match[2];
      $mod   = $match[3];
      if ($previous_layer != $layer) {
	$inc++;
	$file = $file_array[$inc];
	$previous_layer = $layer;
      }

      $sum = 0;
      $items = 0;
      $first = 0;
      $min = 100;
      $max = 0;
    } else {
      $items++;
      $sum += $line;
      if (floatval($line) > $max)
	$max = floatval($line);
      if (floatval($line) < $min)
	$min = floatval($line);
    }
  }
  fprintf($file, "%2.2f %4s  %2.2f\n", ($sum/$items), $RB*2, $max-$min);
}

?>