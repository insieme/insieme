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

#include "insieme/analysis/cba/framework/_forward_decl.h"
#include "insieme/analysis/cba/framework/analysis_type.h"
#include "insieme/analysis/cba/framework/entities/callable.h"

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace analysis {
namespace cba {

	// --- forward definitions of analysis utilized by the framework ---

	struct control_flow_analysis_data;
	struct control_flow_analysis_var;
	extern const control_flow_analysis_data C;
	extern const control_flow_analysis_var  c;

	template<typename C> class Reference;
	struct reference_analysis_data;
	struct reference_analysis_var;
	extern const reference_analysis_data R;
	extern const reference_analysis_var  r;

	template<typename C> class Channel;
	struct channel_analysis_data;
	struct channel_analysis_var;
	extern const channel_analysis_data Ch;
	extern const channel_analysis_var  ch;

	struct functions_analysis_data;
	struct functions_analysis_var;
	extern const functions_analysis_data F;
	extern const functions_analysis_var  f;

	struct context_predecessor_analysis;
	extern const context_predecessor_analysis pred;

	struct Reachable;
	struct reachable_in_analysis;
	extern const reachable_in_analysis Rin;
	struct reachable_out_analysis;
	extern const reachable_out_analysis Rout;

	struct arithmetic_analysis_data;
	struct arithmetic_analysis_var;
	extern const arithmetic_analysis_data A;
	extern const arithmetic_analysis_var  a;

	struct boolean_analysis_data;
	struct boolean_analysis_var;
	extern const boolean_analysis_data B;
	extern const boolean_analysis_var  b;

	struct job_analysis_data;
	struct job_analysis_var;
	extern const job_analysis_data Jobs;
	extern const job_analysis_var  jobs;

	struct thread_group_analysis_data;
	struct thread_group_analysis_var;
	extern const thread_group_analysis_data ThreadGroups;
	extern const thread_group_analysis_var  threadGroups;

	// -- memory location data analysis --

	template<typename A> struct location_data_in_analysis;
	template<typename A> struct location_data_tmp_analysis;
	template<typename A> struct location_data_out_analysis;

	template<typename A> const location_data_in_analysis<A>& Sin();
	template<typename A> const location_data_tmp_analysis<A>& Stmp();
	template<typename A> const location_data_out_analysis<A>& Sout();

} // end namespace cba
} // end namespace analysis
} // end namespace insieme
