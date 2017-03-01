/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#ifndef _UPLINK_ALARM_H
#define _UPLINK_ALARM_H

void uplink_alarm_init(unsigned long delta);
int uplink_wait_for_alarm(void);

#endif
