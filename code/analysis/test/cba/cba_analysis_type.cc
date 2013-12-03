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

#include <gtest/gtest.h>

#include "insieme/core/ir.h"
#include "insieme/core/ir_address.h"
#include "insieme/analysis/cba/framework/analysis_type.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	template<typename C> class DummyGenerator {};

	#define EXPECT_EQ_TYPE(_T1,_T2) \
		EXPECT_EQ(typeid(_T1), typeid(_T2)) << typeid(_T1).name() << " vs. " << typeid(_T2).name();

	TEST(CBA_Analysis_Types, BasicProperties) {

		// this mostly checks whether type traits are working - if it compiles it is fine

		// check the lattice
		typedef typename lattice<set_analysis<int, DummyGenerator>, default_config>::type::value_type int_set;
		EXPECT_EQ_TYPE(std::set<int>, int_set);

		// check the generator
		typedef typename generator<set_analysis<int, DummyGenerator>, default_config>::type dummy_gen;
		EXPECT_EQ_TYPE(DummyGenerator<DefaultContext>, dummy_gen);

		// check the parameters
		typedef typename params<set_analysis<int, DummyGenerator>, default_config>::type param_type;
		typedef std::tuple<AnalysisType,Label,DefaultContext> should_param_type;
		EXPECT_EQ_TYPE(should_param_type, param_type);

	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme
