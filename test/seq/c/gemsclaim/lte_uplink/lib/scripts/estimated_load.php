#!/usr/bin/php
<?php
/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

$eitems = 0;
$mitems = 0;
$estimate = 0;
$measurement = 0;
$time = 1;
$errormax = 0;
$errorsum = 0;
$erroritems = 0;

$estimated_load = 0;
$measured_load = 0;
$iterations = 0;

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
      if ($eitems & $mitems) {
	$error = abs($estimate/$eitems/100 - $measurement/$mitems/100);
	$errorsum += $error;
	$erroritems++;
	if ($errormax < $error)
	  $errormax = $error;
	$estimated_load += $estimate/$eitems/100;
	$measured_load +=  $measurement/$mitems/100;
	$iterations++;
	fprintf($file, "%3.0f\t%3.2f\t%3.2f\t%1.3f\n", $time, $estimate/$eitems/100, $measurement/$mitems/100, $error);
      }
      $time++;
      $eitems = 0;
      $mitems = 0;
      $estimate = 0;
      $measurement = 0;
    } else if (preg_match("/Load: +([0-9]*)/", $line, $match)) {
      $eitems++;
      $estimate += $match[1];
    } else {
      $mitems++;
      $measurement += $line;
    }
  }
  printf("Max error: %f, Average error: %f\n", $errormax, $errorsum/$erroritems);
  printf("Estimated average load: %f\n", $estimated_load/$iterations);
  printf("Measured average load: %f\n", $measured_load/$iterations);
}
?>
