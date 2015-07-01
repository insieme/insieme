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
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme::core;
using namespace insieme::analysis;

TEST(DataDependence, Unique) {
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

	NodeAddress orig=NodeAddress(fragment);               // the original code from above
	insieme::analysis::DataDependence dep(orig);          // parse the original code for variables
	std::vector<VariableAddress> alldefs=dep.getDefs();   // get all variable definitions

	// compare every variable definition with every other definition
	std::vector<std::pair<VariableAddress, VariableAddress> > same;
	for (unsigned int one=0; one<alldefs.size(); one++)
		for (unsigned int two=one+1; two<alldefs.size(); two++)
			if (alldefs[one].getAddressedNode() == alldefs[two].getAddressedNode())
				same.push_back(std::pair<VariableAddress, VariableAddress>(alldefs[one], alldefs[two]));

	if (same.size()) {
		std::cout << "In " << alldefs.size() << " defs, there are " << same.size() << " identical variable pairs:" << std::endl;
		for (auto p: same) {
			boost::optional<NodeAddress> pfx=DataDependence::commonAncestor(p.first, p.second);
			if (pfx) {
				std::cout << "\t"
				          << p.first  << "(" << p.first.getDepth()- pfx->getDepth() << ") : "
				          << p.second << "(" << p.second.getDepth()-pfx->getDepth() << ")";
				//DataDependence::printNode(*pfx);
				std::cout << std::endl;
			}
		}
	}
	EXPECT_TRUE(same.size()==0);
}
