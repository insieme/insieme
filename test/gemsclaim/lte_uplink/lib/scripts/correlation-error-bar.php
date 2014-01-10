#!/usr/bin/php
<?php
/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

if ($argc == 2) {
  $raw = fopen($argv[1], "r");
} else {
  echo "You need to give the file to parse as argument\n";
  exit();
}

if ($raw) {
  $RB  = 2;
  $MIN = 200;
  $MAX = 0;
  $SUM = 0;
  $ITEM = 0;

  while (($line=fgets($raw)) !== false) {
    if (preg_match("/([0-9\.]*) *([0-9]*) *([0-9\.]*)/", $line, $match)) {
      if ($RB != $match[2]) {
	echo $RB ." " .$SUM/$ITEM ." " .$MIN ." " .$MAX ."\n";
	$RB  = $match[2];
	$MIN = 100;
	$MAX = 0;
	$SUM = 0;
	$ITEM = 0;
      }
      if ($match[1] == 0) {
	continue;
      }
      if ($MIN > $match[1]) {
	$MIN = $match[1];
      }
      if ($MAX < $match[1]) {
	$MAX = $match[1];
      }
      $SUM += $match[1];
      $ITEM++;
    }
  }
  echo $RB ." " .$SUM/$ITEM ." " .$MIN ." " .$MAX ."\n";
}
?>