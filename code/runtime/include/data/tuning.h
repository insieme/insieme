/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#pragma once

#include "declarations.h"
#include "irt_inttypes.h"

/**
 * Required improvements:
 * 	- metrics and parameters should be handled within tables + IDs
 * 		- simplifies identification
 * 		- fixes memory management
 * 		- copying ID easier than instances
 * 		- Problem: requires all metrics to be fixed statically (cannot be constructed dynamically)
 */


/* --- subject description --- */

enum irt_subject_type {
	IRT_SUBJECT_PROGRAM,
	IRT_SUBJECT_REGION,
	IRT_SUBJECT_VARIABLE,
	IRT_SUBJECT_RUNTIME
};

typedef struct {
	uint16 program_id;
} irt_subject_program;

typedef struct {
	uint16 program_id;						// < the ID of the context
	irt_wi_implementation_id work_item;		// < the ID of the work item
	uint16 variant;							// < the index of the selected implementation
	uint16 fragment;						// < the code fragment within the implementation
} irt_subject_region;

typedef struct {
	uint16 program_id;
	irt_wi_implementation_id work_item;
	uint16 implementation;
	uint16 variable;
} irt_subject_variable;

typedef struct {
	// no distinct fields yet
} irt_subject_runtime;

typedef struct {
	enum irt_subject_type subject_type;
	union {
		irt_subject_program program;
		irt_subject_region region;
		irt_subject_variable variable;
		irt_subject_runtime runtime;
	};
} irt_subject;

/* --- value description --- */

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

// Problem: metric should have an ID => more efficient handling

typedef int irt_value_type_id;

typedef enum {
	ATOMIC_METRIC, COMPOSED_METRIC
} irt_metric_kind;

typedef struct _irt_metric_atom {
	// only use id => rest in table
	const char* name;
	unsigned sample_resolution; /* < order of magnitude in ns */
} irt_metric_atom;

typedef enum {
	OP_AVG, OP_SUM, OP_MAX, OP_MIN, OP_MOVING_AVG, OP_VARIANCE, OP_DIV
} irt_metric_compose_op;

typedef struct _irt_metric_composed {
	irt_metric_compose_op operation;
//	irt_metric* metricA;
//	irt_metric* metricB;
} irt_metric_composed;

typedef struct {
	irt_metric_kind kind;
	irt_value_type_id value_type;
	union {
		irt_metric_atom atom;
		irt_metric_composed composed;
	};
} irt_metric;


/* --- parameters --- */

typedef struct {
	const char* name;
	irt_type_id type;
} irt_parameter;


/* --- time constraints --- */

typedef uint64 irt_time;

typedef enum {
	IRT_TC_NOW, IRT_TC_LATEST, IRT_TC_LAST_BEFORE, IRT_FIRST_AFTER, IRT_TC_BETWEEN
} irt_time_constraint_kind;

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
irt_tuning_error_code irt_list_params(const irt_subject* subject, irt_parameter out_param[], unsigned* out_num, unsigned max, unsigned offset);

//irt_tuning_error_code irt_list_params(const irt_subject* subject, const irt_parameter(* out_params)[], unsigned* out_num);


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
irt_tuning_error_code irt_list_metrics(const irt_subject* subject, irt_metric out_metric[], unsigned* num, unsigned max, unsigned offset);

/**
 * The main querying function allowing to obtain values for metrics offered by a subject.
 *
 * @param subject ... the (single) subject for which information is requested
 * @param metrics ... the list of metrics requested
 * @param value   ... the target to which the obtained information should be written to
 * @param n       ... the number of requested metrics
 * @param time    ... the time constraints for the requested data (where applicable)
 */
irt_tuning_error_code irt_get_data(const irt_subject* subject, const irt_metric metrics[], irt_value out_value[], unsigned n, const irt_time_constraint* time);

/**
 * Allows to obtain the current state of the parameters offered by a subject.
 *
 * @param subject ... the (single) subject which's parameters should be read
 * @param param   ... the list of parameters requested
 * @param value   ... the target to which the obtained information should be written to
 * @param n       ... the number of requested parameters
 */
irt_tuning_error_code irt_get_params(const irt_subject* subject, const irt_parameter params[], irt_value value[], unsigned n);

/**
 * Allows to update the parameter state of a single subject.
 *
 * @param subject ... the (single) subject which's parameters should be updated
 * @param param   ... the list of parameters to be updated
 * @param value   ... the new values to be assigned to the parameters
 * @param n       ... the number of parameters to be updated
 */
irt_tuning_error_code irt_set_params(const irt_subject* subject, const irt_parameter params[], const irt_value value[], unsigned n);

