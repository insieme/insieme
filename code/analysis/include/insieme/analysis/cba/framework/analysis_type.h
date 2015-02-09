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

#include <typeindex>
#include <string>
#include <functional>

#include <boost/noncopyable.hpp>

#include "insieme/utils/constraint/lattice.h"

#include "insieme/analysis/cba/framework/_forward_decl.h"
#include "insieme/analysis/cba/framework/context.h"
#include "insieme/analysis/cba/framework/entities/data_value.h"

namespace insieme {
namespace analysis {
namespace cba {

	using std::string;

	template<typename Context>
	struct analysis_config {
		typedef Context context_type;
	};

	struct default_config : public analysis_config<DefaultContext> {};


	// analysis type traits

	// a type trait to extract the lattice type from a given analysis and configuration
	template<typename A, typename C = default_config> struct lattice {
		typedef typename A::template lattice<C>::type type;
	};

	// a type trait to extract the generator type from a given analysis and configuration
	template<typename A, typename C = default_config> struct generator {
		typedef typename A::template generator<C>::type type;
	};

	template<typename A, typename C = default_config> struct params {
		typedef typename A::template params<C>::type type;
	};

	// a type trait obtaining the operation to be utilized for merging multiple sequential control flows
	template<typename A, typename C> struct one_meet_assign_op_type {
		typedef typename A::template one_meet_assign_op_type<C>::type type;
	};

	// a type trait obtaining the operation to be utilized for merging multiple parallel control flows
	template<typename A, typename C> struct all_meet_assign_op_type {
		typedef typename A::template all_meet_assign_op_type<C>::type type;
	};


	// analysis utilities

	typedef std::type_index AnalysisType;

	const string& getAnalysisName(AnalysisType index);

	template<typename T>
	const string& getAnalysisName() {
		return getAnalysisName(AnalysisType(typeid(T)));
	}

	namespace detail {
		void registerAnalysisName(AnalysisType index, const string& name);
	}



	// -- utility definitions --


	template<
		typename L,
		template<typename C> class G,
		typename ... Params
	>
	struct analysis_base {
		template<typename C> struct lattice   { typedef L type; };
		template<typename C> struct generator { typedef G<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<Params...> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename L::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename L::meet_assign_op_type type; };
	};


	// -- set analysis --

	template<
		typename E,
		template<typename C> class G
	>
	struct set_analysis : public analysis_base<utils::constraint::SetLattice<E>, G> {
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type> type; };
	};


	// -- data analysis --

	template<
		typename E,
		template<typename C> class G,
		template<typename L> class StructureLattice = FirstOrderStructureLattice
	>
	struct data_analysis : public analysis_base<StructureLattice<utils::constraint::SetLattice<E>>, G> {
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type> type; };
	};


	// -- context-sensitive analysis results --

	template<
		template<typename C> class E,
		template<typename C> class G
	>
	struct dependent_set_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::SetLattice<E<typename C::context_type>> type; };
		template<typename C> struct generator { typedef G<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
	};


	template<
		template<typename C> class E,
		template<typename C> class G,
		template<typename L> class StructureLattice = FirstOrderStructureLattice
	>
	struct dependent_data_analysis {
		template<typename C> struct lattice   { typedef StructureLattice<utils::constraint::SetLattice<E<typename C::context_type>>> type; };
		template<typename C> struct generator { typedef G<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
	};


	// -- location state analysis --

	template<typename C> class Location;

	template<
		template<typename C> class E,
		template<typename C> class G
	>
	struct location_based_set_analysis {
		template<typename C> struct lattice   { typedef utils::constraint::SetLattice<E<typename C::context_type>> type; };
		template<typename C> struct generator { typedef G<typename C::context_type> type; };
		template<typename C> struct params    { typedef std::tuple<AnalysisType, Label, typename C::context_type, Location<typename C::context_type>> type; };
		template<typename C> struct one_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
		template<typename C> struct all_meet_assign_op_type { typedef typename lattice<C>::type::meet_assign_op_type type; };
	};


	template<typename A>
	A registerAnalysis(const string& name) {
		detail::registerAnalysisName(typeid(A), name);
		return A();
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
