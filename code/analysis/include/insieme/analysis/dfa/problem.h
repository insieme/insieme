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

#include <memory>

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/lattice.h"

#include "insieme/analysis/defuse_collect.h"

#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace analysis {
namespace dfa {


// Defines possible direction for dataflow analysis solver
struct ForwardAnalysisTag { };

struct BackwardAnalysisTag { };
// maybe bidirectional analysis


template <class Impl, class D, class E, template <class> class Cont>
class Problem {

	// Builds the lattice 
	void init_internal() {

		extracted = extract(E(), cfg, static_cast<Impl&>(*this));

		lattice_ptr = std::make_shared<LowerSemilattice<container_type>>(
				container_type(extracted), 
					top(), 
					bottom(), 
					std::bind(
						std::mem_fn(&Impl::meet), 
						static_cast<const Impl&>(*this), 
						std::placeholders::_1, 
						std::placeholders::_2
					)
				);
	}

public:

	typedef typename container_type_traits<E>::type extract_type;

	typedef Cont<extract_type> container_type;

	typedef typename container_type::value_type value_type;

	typedef D direction_tag;


	Problem(const CFG& cfg) : cfg(cfg)  { }

	void initialize() { init_internal(); }

	virtual value_type init() const = 0;

	virtual value_type top() const = 0;

	virtual value_type bottom() const = 0;

	virtual value_type meet(const value_type& lhs, const value_type& rhs) const = 0;

	virtual value_type transfer_func(const value_type& in, const cfg::BlockPtr& block) const = 0;

	const extract_type& getExtracted() const { return extracted; }

	const CFG& getCFG() const { return cfg; }

	const LowerSemilattice<container_type>& getLattice() const { 
		assert(lattice_ptr && "Dataflow Problem not correctly initialized");
		return *lattice_ptr; 
	}

protected:
	const CFG& cfg;
	extract_type extracted;
	std::shared_ptr<LowerSemilattice<container_type>> lattice_ptr;
};



} // end dfa namespace 
} // end analysis namespace 
} // end insieme namespace 
