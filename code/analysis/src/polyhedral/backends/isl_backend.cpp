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

// Utility function used to print to a stream the ISL internal representation of a set
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

// Utility function used to print to an output stream the ISL internal representation of maps (or
// relations)
void printIslMap(std::ostream& out, isl_ctx* ctx, isl_map* map) {
	isl_printer* printer = isl_printer_to_str(ctx);
	isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
	isl_printer_set_indent(printer, 1);
	isl_printer_print_map(printer, map);
	isl_printer_flush(printer);
	char* str =  isl_printer_get_str(printer);
	out << str;
	free(str);
	isl_printer_free(printer);
}

isl_constraint* convertConstraint( isl_ctx *islCtx, isl_dim* dim, const Constraint& constraint, const isl_dim_type& type) {
	isl_constraint* islCons = NULL;

	isl_int intVal;
	isl_int_init(intVal);
	
	//std::cout  << "Normalized constrinat " << constraint << std::endl;
	islCons = (constraint.getType() == Constraint::EQ) ? 
				isl_equality_alloc(isl_dim_copy(dim)) : isl_inequality_alloc(isl_dim_copy(dim));
	
	const AffineFunction& af = constraint.getAffineFunction();
	size_t pos=0, sep=af.getIterationVector().getIteratorNum(), size=af.getIterationVector().size();

	for(AffineFunction::iterator it=af.begin(), end=af.end(); it!=end; ++it, ++pos) {
		AffineFunction::Term&& t = *it;
		if(t.second == 0) {	continue; }

		isl_int_set_si(intVal, t.second);
		if (pos < sep) {
			isl_constraint_set_coefficient(islCons, type, pos, intVal);
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

bool isNormalized(const Constraint& c) {
	return c.getType() == Constraint::EQ || c.getType() == Constraint::GE;
}

isl_basic_set* setFromConstraint(isl_ctx* islCtx, isl_dim* dim, const Constraint& c) {

	// check whether the constraint is properly normalize 
	assert(isNormalized(c) && "Constraint not normlized");

	// Create an ISL basic_set 
	isl_basic_set* bset = isl_basic_set_universe( isl_dim_copy(dim) );

	// Create the ISL constraint 
	isl_constraint* cons = convertConstraint( islCtx, dim, c, isl_dim_set);
	
	// Add the constraint to the basic_set
	return isl_basic_set_add_constraint( bset, cons );
}

// Visits the Constraint combiner and builds the corresponding ISL set 
struct ISLConstraintConverterVisitor : public ConstraintVisitor {

	isl_ctx* ctx;
	isl_dim* dim;
	
	isl_set* curr_set;

	ISLConstraintConverterVisitor(isl_ctx* ctx, isl_dim* dim) : ctx(ctx), dim(dim) { }

	void visit(const RawConstraintCombiner& rcc) { 
		const Constraint& c = rcc.getConstraint();
		if ( isNormalized(c) ) {
			isl_basic_set* bset = setFromConstraint(ctx, dim, c);
			curr_set = isl_set_from_basic_set( bset );
			return;
		}
		normalize(c)->accept(*this);
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

		curr_set = bcc.isConjunction() ? isl_set_intersect(lhs, rhs) : isl_set_union(lhs, rhs);
	}

	isl_set* getResult() const { return curr_set; }

};

template <class IterT>
void setVariableName(isl_dim* dim, const isl_dim_type& type, IterT const& begin, IterT const& end) {
	for(IterT it = begin; it != end; ++it) {
		isl_dim_set_name(dim, type, std::distance(begin, it), it->getVariable()->toString().c_str());
	}
}

} // end anonynous namespace

//==== IslSet ====================================================================================

IslSet::IslSet(IslContext& ctx, const IterationVector& iterVec) : Set(ctx, iterVec) {
	// Build the dim object
	dim = isl_dim_set_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum() );

	// Set the names for the iterators of this dim
	setVariableName(dim, isl_dim_set, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(dim, isl_dim_param, iterVec.param_begin(), iterVec.param_end());

	// creates an universe set containing the dimensionatility of the iteration vector
	set = isl_set_universe( isl_dim_copy(dim) );
}

std::ostream& IslSet::printTo(std::ostream& out) const {
	printIslSet(out, ctx.getRawContext(), set); 
	return out;
}

void IslSet::applyConstraint(const ConstraintCombinerPtr& cc) {
	ISLConstraintConverterVisitor ccv(ctx.getRawContext(), dim);
	cc->accept(ccv);

	set = isl_set_intersect(set, ccv.getResult());
}

//==== IslMap ====================================================================================

IslMap::IslMap(IslContext& ctx, const IterationVector& iterVec, const AffineSystem& affSys) : Map(ctx, iterVec) {
	// Build the dim object
	dim = isl_dim_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum(), affSys.size());

	// Set the names for the iterators of this dim
	setVariableName(dim, isl_dim_in, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(dim, isl_dim_param, iterVec.param_begin(), iterVec.param_end());
	
	// creates an universe set containing the dimensionatility of the iteration vector
	size_t idx=0;

	isl_basic_map* bmap = isl_basic_map_universe( isl_dim_copy(dim) );
	for(AffineSystem::AffineList::const_iterator it=affSys.begin(), end=affSys.end(); it!=end; ++it, ++idx) {
		isl_constraint* cons = convertConstraint(ctx.getRawContext(), dim, Constraint(*it, Constraint::EQ), isl_dim_in);
		// because each constraint is referring to a particular out dimension of the affine system,
		// we have to sed to 1 the particular out index 
		isl_int intVal;
		isl_int_init(intVal);
		isl_int_set_si(intVal, -1);
		isl_constraint_set_coefficient(cons, isl_dim_out, idx, intVal);
		isl_int_clear(intVal);

		// Add constraint to the basic map
		bmap = isl_basic_map_add_constraint(bmap, cons);
	}
	// convert the basic map into a map
	map = isl_map_from_basic_map(bmap);
}

std::ostream& IslMap::printTo(std::ostream& out) const {
	printIslMap(out, ctx.getRawContext(), map); 
	return out;
}

void IslMap::intersect(const Set<IslContext>& set) {
	map = isl_map_intersect_domain( map, isl_set_copy(static_cast<const IslSet&>(set).set) );
}

} // end backends namespace 
} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 
