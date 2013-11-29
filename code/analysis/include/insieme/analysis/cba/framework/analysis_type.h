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

#include <string>
#include <functional>

#include <boost/noncopyable.hpp>

#include "insieme/utils/constraint/lattice.h"

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


	// analysis utilities

	const string& getAnalysisName(std::type_index index);

	template<typename T>
	const string& getAnalysisName() {
		return getAnalysisName(std::type_index(typeid(T)));
	}

	namespace detail {
		void registerAnalysisName(std::type_index index, const string& name);
	}



	// -- utility definitions --


	template<
		typename L,
		template<typename C> class G
	>
	struct analysis_base {
		template<typename C> struct lattice   { typedef L type; };
		template<typename C> struct generator { typedef G<typename C::context_type> type; };
	};


	// -- set analysis --

	template<
		typename E,
		template<typename C> class G
	>
	struct set_analysis : public analysis_base<utils::constraint::SetLattice<E>, G> {};


	// -- data analysis --

	template<
		typename E,
		template<typename C> class G,
		template<typename L> class StructureLattice = FirstOrderStructureLattice
	>
	struct data_analysis : public analysis_base<StructureLattice<utils::constraint::SetLattice<E>>, G> {};


	// -- context-sensitive analysis results --

	template<
		template<typename C> class E,
		template<typename C> class G,
		template<typename L> class StructureLattice = FirstOrderStructureLattice
	>
	struct dependent_data_analysis {
		template<typename C> struct lattice   { typedef StructureLattice<utils::constraint::SetLattice<E<typename C::context_type>>> type; };
		template<typename C> struct generator { typedef G<typename C::context_type> type; };
	};


	// -- location state analysis --

	template<
		typename BaseAnalysis,							// the analysis this analysis is extending
		template<typename C, typename A> class G		// the location state generator class (has additional parameter)
	>
	struct location_state_analysis {
		template<typename C> struct lattice   { typedef typename cba::lattice<BaseAnalysis,C>::type type; };
		template<typename C> struct generator { typedef G<typename C::context_type, BaseAnalysis> type; };
	};


	template<typename A>
	A registerAnalysis(const string& name) {
		detail::registerAnalysisName(typeid(A), name);
		return A();
	}


	// ----------- analysis types ------------------

	/**
	 * An abstract base class for analysis types. Instances are expected
	 * to be immutable global constants.
	 */
	class AnalysisTypeBase : public boost::noncopyable, public utils::Printable {

		/**
		 * The name of this set for printing and debugging issues.
		 */
		const string name;

	protected:

		AnalysisTypeBase(const string& name) : name(name) {}

	public:

		const string& getName() const {
			return name;
		}

		bool operator==(const AnalysisTypeBase& other) const {
			// the identity of a analysis type is fixed by its address
			return this == &other;
		}

		bool operator!=(const AnalysisTypeBase& other) const {
			return !(*this == other);
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << name;
		}

	};

	// all set types are global constants => plain pointers can be used safely
	typedef const AnalysisTypeBase* AnalysisTypePtr;

	/**
	 * A special type of analysis type fixing the value type of the analysis results and
	 * class definitions implementing analysis.
	 *
	 * @tparam L the lattice forming the result of this analysis
	 * @tparam G the constraint generator implementation implementing the represented analysis.
	 */
	template<
		typename L,
		template<typename C> class G
	>
	struct AnalysisType : public AnalysisTypeBase, public analysis_base<L,G> {

		// expose member types
		typedef L lattice_type;
		template<typename T> struct resolver_type { typedef G<T> type; };

		/**
		 * A simple constructor just forwarding the name of the resulting set.
		 */
		AnalysisType(const string& name) : AnalysisTypeBase(name) {}

	};


	template<
		typename E,
		template<typename C> class G,
		template<typename L> class StructureLattice = FirstOrderStructureLattice
	>
	struct DataAnalysisType : public AnalysisType<StructureLattice<utils::constraint::SetLattice<E>>, G> {

		/**
		 * A simple constructor just forwarding the name of the resulting set.
		 */
		DataAnalysisType(const string& name) : AnalysisType<StructureLattice<utils::constraint::SetLattice<E>>, G>(name) {}

	};

	// TODO: remove this set based analysis type once it is no longer needed (just introduced for
	//			refactoring)
	template<
		typename E,
		template<typename C> class G
	>
	struct SetBasedAnalysisType : public AnalysisType<utils::constraint::SetLattice<E>, G> {

		/**
		 * A simple constructor just forwarding the name of the resulting set.
		 */
		SetBasedAnalysisType(const string& name) : AnalysisType<utils::constraint::SetLattice<E>,G>(name) {}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
