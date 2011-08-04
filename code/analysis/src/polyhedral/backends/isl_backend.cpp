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

#include "insieme/analysis/polyhedral/backends/isl_backend.h"

#include "insieme/core/expressions.h"

#include "insieme/utils/logging.h"

#include "isl/constraint.h"

namespace insieme {
namespace analysis {
namespace poly {
namespace backend {

namespace {

void printIslSet(std::ostream& out, isl_ctx* ctx, isl_set* set) {
	isl_printer* printer = isl_printer_to_str(ctx);
	isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
	isl_printer_set_indent(printer, 1);
	isl_printer_print_set(printer, set);
	isl_printer_flush(printer);
	char* str = isl_printer_get_str(printer);
	out << str;
	free(str); // free the allocated string by the library
	isl_printer_free(printer);
}

} // end anonymous namespace 

isl_constraint* convertConstraint(isl_ctx *islCtx, struct isl_dim* dim, const Constraint& constraint) {
	isl_constraint* islCons = NULL;

	isl_int intVal;
	isl_int_init(intVal);
	
	//std::cout  << "Normalized constrinat " <<  constraint << std::endl;

	islCons = (constraint.getType() == Constraint::EQ) ? 
				isl_equality_alloc(isl_dim_copy(dim)) : isl_inequality_alloc(isl_dim_copy(dim));
	
	const AffineFunction& af = constraint.getAffineFunction();
	size_t pos=0, sep=af.getIterationVector().getIteratorNum(), size=af.getIterationVector().size();

	for(AffineFunction::iterator it=af.begin(), end=af.end(); it!=end; ++it, ++pos) {
		AffineFunction::Term&& t = *it;
		if(t.second == 0) {	continue; }

		isl_int_set_si(intVal, t.second);
		if (pos < sep) {
			isl_constraint_set_coefficient(islCons, isl_dim_set, pos, intVal);
			continue;
		}

		if (pos >= sep && pos < size-1) {
			isl_constraint_set_coefficient(islCons, isl_dim_param, pos-sep, intVal);
			continue;
		}
		isl_constraint_set_constant(islCons, intVal);
	}
	isl_int_clear(intVal);
	assert(islCons != NULL && "Constraint not correctly initialized");
	return islCons;
}

isl_basic_set* setFromConstraint(isl_ctx* islCtx, isl_dim* dim, const Constraint& c) {
	// Create an ISL basic_set 
	isl_basic_set* bset = isl_basic_set_universe( isl_dim_copy(dim) );

	// Because ISL only handle equalities and inequalities in the form >=, we have to normalize the
	// constraint to fit this specification

	// Create the ISL constraint 
	isl_constraint* cons = convertConstraint( islCtx, dim, c.normalize() );
	
	// Add the constraint to the basic_set
	return isl_basic_set_add_constraint( bset, cons );
}

IslSet::IslSet(IslContext& ctx, const IterationVector& iterVec) : Set(ctx, iterVec) {
	// Build the dim object
	dim = isl_dim_set_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum() );

	// Set the names for the iterators of this dim
	for(IterationVector::iter_iterator it = iterVec.iter_begin(), end = iterVec.iter_end(); it != end; ++it) {
		isl_dim_set_name(dim, isl_dim_set, std::distance(iterVec.iter_begin(), it), it->getVariable()->toString().c_str());
	}

	// Set the names for the parameters of this dim
	for(IterationVector::param_iterator it = iterVec.param_begin(), end = iterVec.param_end(); it != end; ++it) {
		isl_dim_set_name(dim, isl_dim_param, std::distance(iterVec.param_begin(), it), it->getVariable()->toString().c_str());
	}

	// creates an universe set containing the dimensionatility of the iteration vector
	set = isl_set_universe( isl_dim_copy(dim) );
}


std::ostream& IslSet::printTo(std::ostream& out) const {
	printIslSet(out, ctx.getRawContext(), set); 
	return out;
}

void IslSet::addConstraint(const Constraint& c) {
	isl_basic_set* bset = setFromConstraint(ctx.getRawContext(), dim, c);
	isl_set *tmp_set = isl_set_from_basic_set( bset );

	// Intersect the current set with the new constraint
	set = isl_set_intersect(tmp_set, set);
}

namespace {

// Visits the Constraint combiner and builds the corresponding ISL set 
struct ISLConstraintConverterVisitor : public ConstraintVisitor {

	isl_ctx* ctx;
	isl_dim* dim;
	
	isl_set* curr_set;

	ISLConstraintConverterVisitor(isl_ctx* ctx, isl_dim* dim) : ctx(ctx), dim(dim) { }

	void visit(const RawConstraintCombiner& rcc) { 
		isl_basic_set* bset = setFromConstraint(ctx, dim, rcc.getConstraint());
		curr_set = isl_set_from_basic_set( bset );
	}

	void visit(const NegatedConstraintCombiner& ucc) {
		ConstraintVisitor::visit( ucc.getSubConstraint() );
		// in curr_set we have the set coming from the sub constraint, we have to negate it 
		isl_basic_set* universe = isl_basic_set_universe( isl_dim_copy(dim) );

		curr_set = isl_set_subtract( isl_set_from_basic_set(universe), curr_set );
	}
	
	void visit(const BinaryConstraintCombiner& bcc) {
		bcc.getLHS()->accept(*this);
		isl_set* lhs = curr_set;
		bcc.getRHS()->accept(*this);
		isl_set* rhs = curr_set;

		curr_set = bcc.isConjunction() ? isl_set_union(lhs, rhs) : isl_set_intersect(lhs, rhs);
	}

	isl_set* getResult() const { return curr_set; }

};

} // end anonymous namespace


void IslSet::addConstraint(const ConstraintCombinerPtr& cc) {
	ISLConstraintConverterVisitor ccv(ctx.getRawContext(), dim);
	ccv.visit(cc);

	set = isl_set_intersect(set, ccv.getResult());
}

} // end backends namespace 
} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 
