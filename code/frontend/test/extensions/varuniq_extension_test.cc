/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include <sstream>
#include "insieme/analysis/datadep.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/extensions/varuniq_extension.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::frontend::extensions;

TEST(VarUniq, Simple) {
	NodeManager man;
	IRBuilder builder(man);

	NodePtr fragment = builder.parseStmt(R"(
	            {
					let twoElem = struct{int<4> int; real<4> float;};
					let tuple = (ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>);
				
					let access = lambda (ref<array<ref<array<twoElem,1>>, 1>> x)->unit {
						for(int<4> i = 0 .. 42 : 1) {
							ref_deref(x[0])[i].int = i;
						}
					};
				
					let writeToTuple = lambda (ref<(ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>)> lt, 
							                   ref<array<ref<array<twoElem,1>>,1>> x)->unit {
						(*x[0])[3].int = 7;
						tuple_ref_elem(lt,0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = x;
					};
				
					let actualWork = lambda (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, 
							                 vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {
					};
				
					let local = lambda (ref<array<real<4>,1>> b, ref<array<twoElem,1>> a, uint<8> c, 
							vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {
						parallel(job([vector_reduction(local_size, 1u, uint_mul) : vector_reduction(local_size, 1u, uint_mul)]
						,	actualWork(a, b, c, local_size, global_size)
						));
					};
				
					let global = lambda (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, 
							vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {
						decl vector<uint<8>,3> groups = vector_pointwise(uint_div)(global_size, local_size);
						parallel(job([vector_reduction(groups, 1u, uint_mul) : vector_reduction(groups, 1u, uint_mul)]
						,	local(b, a, c, local_size, global_size)
						));
					};
				
					let kernelFct = lambda (tuple kernel, vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> int<4> {
						global(
							*(tuple_member_access(kernel, 0u, lit(ref<array<ref<array<twoElem,1>>,1>>))[0]),
							*(tuple_member_access(kernel, 1u, lit(ref<array<ref<array<real<4>,1>>,1>>))[0]),
							*(tuple_member_access(kernel, 2u, lit(ref<array<uint<8>,1>>))[0]),
							local_size, global_size);
				
						return 0;
					};
				
					decl ref<ref<array<twoElem,1>>> a;
					decl ref<ref<array<real<4>,1>>> b;
					decl ref<uint<8>> c;
					decl ref<ref<tuple>> t;
					t = new(undefined((ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>) ));
					ref_deref(a);
					access(scalar_to_array(a));
					tuple_ref_elem(*t,0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = scalar_to_array(a);
					writeToTuple(*t, scalar_to_array(a));
				
					decl vector<uint<8>,3> ls;
					decl vector<uint<8>,3> gs;
				
					kernelFct(**t, ls, gs);
				
					ref_delete(*t);
				}
			)");

	ASSERT_TRUE(fragment);

	NodeAddress orig=NodeAddress(fragment);                      // the original code from above
	insieme::frontend::extensions::VarUniqExtension old(orig);   // parse the original code for variables
	NodeAddress result=old.IR();                                 // replacement of vars
	insieme::analysis::DataDependence now(result);               // parse the resulting code for variables

	// show the changes of the variable uniquification process visually
	std::stringstream strbuf;
	strbuf << "# # # # #   OLD CODE   # # # # #" << std::endl
	       << printer::PrettyPrinter(orig.getAddressedNode()) << std::endl << std::endl
	       << "# # # # #   NEW CODE   # # # # #" << std::endl
	       << printer::PrettyPrinter(result.getAddressedNode()) << std::endl;
	//std::cout << strbuf.str();

	// get all variable definitions from both codes
	std::vector<VariableAddress>
	        old_defs=old.dep.getDefs(),
	        now_defs=now.getDefs();

	// check some boundary conditions
	EXPECT_EQ(old.dep.size(), now.size());         // the number of variables must match in both codes
	EXPECT_EQ(old_defs.size(), now_defs.size());   // the number of variable definitions must match in both codes

	// basic test that should not fail: the number of definitions must match the highest variable ID
	unsigned int old_maxid=VarUniqExtension::findMaxID(old_defs);
	unsigned int now_maxid=VarUniqExtension::findMaxID(now_defs);
	EXPECT_EQ(now_maxid+1, now_defs.size());

	// in the original code, we expect some vacant and some double booked IDs
	bool old_allset=true;
	unsigned int old_doublebooked=0;
	for (unsigned int old_i=0; old_i<old_maxid; ++old_i) {
		unsigned int count=old.dep.getDefs(old_i).size();
		old_allset=old_allset && count;
		if (count>1) old_doublebooked+=count;
	}
	EXPECT_FALSE(old_allset);
	EXPECT_TRUE(old_doublebooked);

	// in the new code, every variable ID should be used but not more than once
	bool now_allset=true;
	unsigned int now_doublebooked=0;
	for (unsigned int now_i=0; now_i<now_maxid; ++now_i) {
		unsigned int count=now.getDefs(now_i).size();
		if (!count) { LOG(ERROR) << "variable v" << now_i << " not found!" << std::endl; }
		now_allset=now_allset && count;
		if (count>1) now_doublebooked+=count;
	}
	EXPECT_TRUE(now_allset);
	EXPECT_FALSE(now_doublebooked);
}
