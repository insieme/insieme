/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 */

#pragma once
#ifndef __GUARD_DATA_METRIC_TABLE_H
#define __GUARD_DATA_METRIC_TABLE_H

#include "tuning.h"


/**
 * Defines global names for the indices of atomic metrics. Those indices
 * may change as the list of supported metrics is changing (however, only
 * during compile time).
 */
enum {
#define METRIC(id, name, type, res, desc) IRT_METRIC_##name##_INDEX,
#include "metric.def"
#undef METRIC
};


/**
 * A global list of instances of atomic metrics pointers. These metrics
 * can be directly used for assembling lists of metrics to be queried.
 */
#define METRIC(id, name, type, res, desc) irt_metric IRT_METRIC_##name[] = {{ATOMIC_METRIC, {{IRT_METRIC_##name##_INDEX}}}};
#include "metric.def"
#undef METRIC

/**
 * A list of all atomic metric pointers.
 */
const irt_metric* g_all_atomic_metrics[] = {
#define METRIC(id, name, type, res, desc) IRT_METRIC_##name,
#include "metric.def"
#undef METRIC
};

/**
 * Obtain the number of atomic metrics.
 */
const uint16 g_num_atomic_metrics = 0
#define METRIC(id, name, type, res, desc) +1
#include "metric.def"
#undef METRIC
    ;

/**
 * The metric table listing all atomic metrics supported within the system.
 * The IDs used to identify atomic metrics is based on the order of the
 * entries within this table.
 */
const irt_atomic_metric_info g_atomic_metric_table[] = {
#define METRIC(id, name, type, res, desc)                                                                                                                      \
	{ id, type, res, desc }                                                                                                                                    \
	,
#include "metric.def"
#undef METRIC
};


// --------------------------- utilities ------------------------

const irt_atomic_metric_info* irt_get_metric_info(irt_atomic_metric_index metric) {
	return &g_atomic_metric_table[metric];
}


#endif // ifndef __GUARD_DATA_METRIC_TABLE_H
