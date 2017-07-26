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
#ifndef __GUARD_DATA_TUNING_H
#define __GUARD_DATA_TUNING_H

#include "declarations.h"
#include "irt_inttypes.h"
#include "irt_context.h"
#include "worker.h"

/**
 * Required improvements:
 * 	- metrics and parameters should be handled within tables + IDs
 * 		- simplifies identification
 * 		- fixes memory management
 * 		- copying ID easier than instances
 * 		- Problem: requires all metrics to be fixed statically (cannot be constructed dynamically)
 */


/* --- subject description --- */

typedef enum {
	IRT_SUBJECT_PROGRAM,
	IRT_SUBJECT_REGION,
	IRT_SUBJECT_VARIABLE,
	IRT_SUBJECT_WORKER,
	IRT_SUBJECT_RUNTIME,
	// + all other entities of the HW model
} irt_subject_type;

typedef struct { irt_context_id context_id; } irt_subject_program;

typedef struct {
	irt_context_id context_id;          // < the ID of the context
	irt_wi_implementation_id work_item; // < the ID of the work item
	uint16 variant;                     // < the index of the selected implementation
	uint16 fragment;                    // < the code fragment within the implementation
} irt_subject_region;

typedef struct {
	irt_context_id context_id;          // < the ID of the context
	irt_wi_implementation_id work_item; // < the work item implementation ID (impl table)
	uint16 variant;                     // < the variant of the implementation
	uint16 variable;                    // < the index of the variable within the variant
} irt_subject_variable;

typedef struct {
	irt_worker_id worker_id; // < the ID of the addressed worker
} irt_subject_worker;

typedef struct {
	uint16 node_id; // < the ID of the node the runtime is running on
} irt_subject_runtime;

typedef struct {
	irt_subject_type subject_type;
	union {
		irt_subject_program program;
		irt_subject_region region;
		irt_subject_variable variable;
		irt_subject_worker worker;
		irt_subject_runtime runtime;
	};
} irt_subject;

/* --- value description --- */

/**
 * An enumeration used to distinguish value types for metrics.
 */
typedef enum {
	IRT_VT_BOOL,
	IRT_VT_INT8,
	IRT_VT_INT16,
	IRT_VT_INT32,
	IRT_VT_INT64,
	IRT_VT_UINT8,
	IRT_VT_UINT16,
	IRT_VT_UINT32,
	IRT_VT_UINT64,
	IRT_VT_FLOAT,
	IRT_VT_DOUBLE,
	IRT_VT_COMPLEX, // < the value is a pointer to something complex
} irt_value_type;


typedef struct {
	bool valid;
	union {
		bool value_bool;
		int8 value_int8;
		int16 value_int16;
		int32 value_int32;
		int64 value_int64;
		uint8 value_uint8;
		uint16 value_uint16;
		uint32 value_uint32;
		uint64 value_uint64;
		float value_float;
		double value_double;
		void* value_ptr;
	};
} irt_value;

/* --- metric --- */

typedef uint16 irt_atomic_metric_id;

/**
 * The enum used to represent sample resolutions.
 * Resolutions are categorized in order of magnitudes.
 */
typedef enum {
	IRT_SR_1_NS,
	IRT_SR_10_NS,
	IRT_SR_100_NS,
	IRT_SR_1_US,
	IRT_SR_10_US,
	IRT_SR_100_US,
	IRT_SR_1_MS,
	IRT_SR_10_MS,
	IRT_SR_100_MS,
	IRT_SR_1_SEC,
	IRT_SR_10_SEC,
	IRT_SR_STATIC
} irt_sample_resolution;


/**
 * This struct defined the basic properties of atomic
 * metrics offered by the runtime. Instances of this type
 * should be organized within a table and the index of
 * atomic metrices within this table are used to identify
 * those.
 */
typedef struct {
	const irt_atomic_metric_id id;
	const irt_value_type value_type;
	const irt_sample_resolution sample_resolution;
	const char* description;
} irt_atomic_metric_info;

typedef uint16 irt_atomic_metric_index;

typedef enum { ATOMIC_METRIC, COMPOSED_METRIC } irt_metric_kind;

typedef enum {
	OP_AVG,
	OP_MAX,
	OP_MIN, // < single argument connectors
	OP_MOVING_AVG,
	OP_VARIANCE,
	OP_SUM,
	OP_PROD,
	OP_ADD,
	OP_SUB,
	OP_MUL,
	OP_DIV // < binary connectors
} irt_metric_combinator;


typedef struct _irt_metric {
	irt_metric_kind kind; // < determines whether it is an atomic or composed metric
	union {
		struct {
			irt_atomic_metric_index index; // < the index of the atomic metric within the table
		};
		struct {
			irt_metric_combinator combinator; // < the connector used for composing
			struct _irt_metric* metricA;      // < the first sub-metric
			struct _irt_metric* metricB;      // < the second sub-metric
		};
	};
} irt_metric;


/* --- parameters --- */

// TODO: this does not support complex values for parameters (e.g. parameterized policies)

typedef uint16 irt_parameter_id;

// this struct should be used to establish a parameter table within a context
typedef struct {
	const irt_parameter_id id;
	const irt_value_type type;
	const char* desc;
} irt_parameter_info;


/* --- time constraints --- */

typedef uint64 irt_time;

typedef enum { IRT_TC_NOW, IRT_TC_LATEST, IRT_TC_LAST_BEFORE, IRT_FIRST_AFTER, IRT_TC_BETWEEN } irt_time_constraint_kind;

typedef struct {
	irt_time begin;
	irt_time end;
} irt_time_interval;

typedef struct {
	irt_time_constraint_kind kind;
	union {
		irt_time timestamp;
		irt_time_interval interval;
	};
} irt_time_constraint;


// --------------------------------------------------------------------
//    The main functionality to interact with the remaining system
// --------------------------------------------------------------------

typedef enum {

} irt_tuning_error_code;

/**
 * Allows to obtain a list of parameters offered by a certain subject. The function
 * will write a copy of the offered parameters to the given parameter list.
 *
 * @param subject ... a pointer to the requested subject
 * @param param   ... a pointer to the parameters list to be filled
 * @param num     ... will be filled by the number of parameters written to param
 * @param max     ... the maximal number parameters to be written to param (size of param)
 * @param offset  ... the offset to the total list of parameters
 */
irt_tuning_error_code irt_list_params(const irt_subject* subject, irt_parameter_id out_param[], unsigned* out_num, unsigned max, unsigned offset);

/**
 * Obtains a list of metrics offered by a given subject. The function will copy
 * the a section of the corresponding metrics list to the given metric array.
 *
 * @param subject ... a pointer to the corresponding subject
 * @param metric  ... the list of metrics to be written to
 * @param num     ... will be filled by the number of metrics written to metric
 * @param max     ... the maximal number of metrics to be written to the given metric list
 * @param offset  ... the offset starting at
 */
irt_tuning_error_code irt_list_metrics(const irt_subject* subject, const irt_metric* out_metric[], unsigned* num, unsigned max, unsigned offset);

/**
 * The main querying function allowing to obtain values for metrics offered by a subject.
 *
 * @param subject ... the (single) subject for which information is requested
 * @param metrics ... the list of metrics requested
 * @param value   ... the target to which the obtained information should be written to
 * @param n       ... the number of requested metrics
 * @param time    ... the time constraints for the requested data (where applicable)
 */
irt_tuning_error_code irt_get_data(const irt_subject* subject, const irt_metric* metrics[], irt_value out_value[], unsigned n, const irt_time_constraint* time);

/**
 * Allows to obtain the current state of the parameters offered by a subject.
 *
 * @param subject ... the (single) subject which's parameters should be read
 * @param param   ... the list of parameters requested
 * @param value   ... the target to which the obtained information should be written to
 * @param n       ... the number of requested parameters
 */
irt_tuning_error_code irt_get_params(const irt_subject* subject, const irt_parameter_id params[], irt_value value[], unsigned n);

/**
 * Allows to update the parameter state of a single subject.
 *
 * @param subject ... the (single) subject which's parameters should be updated
 * @param param   ... the list of parameters to be updated
 * @param value   ... the new values to be assigned to the parameters
 * @param n       ... the number of parameters to be updated
 */
irt_tuning_error_code irt_set_params(const irt_subject* subject, const irt_parameter_id params[], const irt_value value[], unsigned n);


#endif // ifndef __GUARD_DATA_TUNING_H
