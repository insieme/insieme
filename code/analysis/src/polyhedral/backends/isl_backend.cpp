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
#include "isl/flow.h"
#include "isl/polynomial.h"

#include "barvinok/isl.h"

namespace insieme {
namespace analysis {
namespace poly {

namespace {

// Utility function used to print to a stream the ISL internal representation of a set
void printIslSet(std::ostream& out, isl_ctx* ctx, isl_union_set* set) {
	isl_printer* printer = isl_printer_to_str(ctx);
	isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
	isl_printer_set_indent(printer, 1);
	isl_printer_print_union_set(printer, set);
	isl_printer_flush(printer);
	char* str = isl_printer_get_str(printer);
	out << str;
	free(str); // free the allocated string by the library
	isl_printer_free(printer);
}

// Utility function used to print to an output stream the ISL internal representation of maps (or
// relations)
void printIslMap(std::ostream& out, isl_ctx* ctx, isl_union_map* map) {
	isl_printer* printer = isl_printer_to_str(ctx);
	isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
	isl_printer_set_indent(printer, 1);
	isl_printer_print_union_map(printer, map);
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
		assert(pos < size);
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
		assert(dynamic_cast<const Expr*>(&*it) != NULL && "Element of not Variable type");
		const poly::Expr& var = static_cast<const Expr&>(*it);
		std::ostringstream ss;
		ss << var;
		isl_dim_set_name(dim, type, std::distance(begin, it), ss.str().c_str());
	}
}

} // end anonynous namespace

//==== Set ====================================================================================

Set<IslContext>::Set(
		IslContext& 			ctx, 
		const IterationDomain& 	domain,
		const TupleName&		tuple 
	) : ctx(ctx)
{
	const IterationVector& iterVec = domain.getIterationVector();
	// Build the dim object
	dim = isl_dim_set_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum() );

	// Set the names for the iterators of this dim
	setVariableName(dim, isl_dim_set, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(dim, isl_dim_param, iterVec.param_begin(), iterVec.param_end());
	
	if (tuple.first) {
		ctx.insertTuple( tuple );
		// Set the name of the tuple 
		dim = isl_dim_set_tuple_name(dim, isl_dim_set, tuple.second.c_str());
	}

	if ( domain.isEmpty() ) {
		set = isl_union_set_empty( isl_dim_copy(dim) );
		return;
	}

	if ( domain.isUniverse() ) {
		set = isl_union_set_from_set( isl_set_universe( isl_dim_copy(dim) ) );
		return;
	}

	assert( domain.getConstraint() && "Constraints for this iteration domain cannot be empty" );

	// If a non empty constraint is provided, then add it to the universe set 
	ISLConstraintConverterVisitor ccv(ctx.getRawContext(), dim);
	domain.getConstraint()->accept(ccv);

	isl_set* cset = ccv.getResult();
	size_t pos = 0;
	std::for_each ( iterVec.iter_begin(), iterVec.iter_end(),
		[&]( const Iterator& iter ) {
			// peel out this dimension by projecting it 
			if ( iter.isExistential() ) { cset = isl_set_project_out( cset, isl_dim_set, pos, 1); }
			pos++;
		}
	);

	isl_dim_free(dim);
	dim = isl_set_get_dim( cset );
	set = isl_union_set_from_set( cset );
}

bool Set<IslContext>::isEmpty() const { return isl_union_set_is_empty(set);	}

void Set<IslContext>::simplify() {
	set = isl_union_set_coalesce( set );
	set = isl_union_set_detect_equalities( set );
}

std::ostream& Set<IslContext>::printTo(std::ostream& out) const {
	printIslSet(out, ctx.getRawContext(), set); 
	return out;
}

//==== Map ====================================================================================

Map<IslContext>::Map(IslContext& 			ctx, 
					 const AffineSystem& 	affSys, 
					 const TupleName&	 	in_tuple, 
					 const TupleName& 		out_tuple 
					) : ctx(ctx)
{
	const IterationVector& iterVec = affSys.getIterationVector();

	// Build the dim object
	dim = isl_dim_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum(), affSys.size());

	// Set the names for the iterators of this dim
	setVariableName(dim, isl_dim_in, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(dim, isl_dim_param, iterVec.param_begin(), iterVec.param_end());

	// Set the input tuple name if specified
	if ( in_tuple.first ) {
		ctx.insertTuple( in_tuple );
		dim = isl_dim_set_tuple_name(dim, isl_dim_in, in_tuple.second.c_str());
	}

	if ( out_tuple.first ) {
		ctx.insertTuple( out_tuple ); 
		dim = isl_dim_set_tuple_name(dim, isl_dim_out, out_tuple.second.c_str());
	}
	
	// creates an universe set containing the dimensionatility of the iteration vector
	size_t idx=0;

	if (affSys.size() == 0) {
		// create an empty map
		map = isl_union_map_from_map( isl_map_empty( isl_dim_copy(dim) ) );
		return;
	}
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

	//size_t pos = 0;
	//std::for_each ( iterVec.iter_begin(), iterVec.iter_end(),
		//[&]( const Iterator& iter ) {
			//// peel out this dimension by projecting it 
			//if ( iter.isExistential() ) { bmap = isl_basic_map_project_out( bmap, isl_dim_in, pos, 1); }
			//pos++;
		//}
	//);

	//isl_dim_free(dim);
	//dim = isl_basic_map_get_dim( bmap );

	// convert the basic map into a map
	map = isl_union_map_from_map(isl_map_from_basic_map(bmap));
}

std::ostream& Map<IslContext>::printTo(std::ostream& out) const {
	printIslMap(out, ctx.getRawContext(), map); 
	return out;
}

void Map<IslContext>::simplify() {
	map = isl_union_map_coalesce( map );
	map = isl_union_map_detect_equalities( map );
}

SetPtr<IslContext> Map<IslContext>::deltas() const {
	
	isl_union_set* deltas = isl_union_map_deltas( isl_union_map_copy(map) );
	return SetPtr<IslContext>(ctx, isl_union_set_get_dim(deltas), deltas);

}

bool Map<IslContext>::isEmpty() const { return isl_union_map_is_empty(map);	}

//==== Sets and Maps operations ===================================================================

template <>
SetPtr<IslContext> 
set_union(IslContext& ctx, const Set<IslContext>& lhs, const Set<IslContext>& rhs) {
	isl_union_set* set = isl_union_set_union(
			isl_union_set_copy( lhs.getAsIslSet() ), isl_union_set_copy( rhs.getAsIslSet() )
	);
	return SetPtr<IslContext>(ctx, isl_union_set_get_dim(set), set);
}

template <>
SetPtr<IslContext> 
set_intersect(IslContext& ctx, const Set<IslContext>& lhs, const Set<IslContext>& rhs) {
	isl_union_set* set = isl_union_set_intersect(
			isl_union_set_copy( lhs.getAsIslSet() ), isl_union_set_copy( rhs.getAsIslSet() )
	);
	return SetPtr<IslContext>(ctx, isl_union_set_get_dim(set), set);
}

template <>
MapPtr<IslContext> 
map_union(IslContext& ctx, const Map<IslContext>& lhs, const Map<IslContext>& rhs) {
	isl_union_map* map = isl_union_map_union( 
			isl_union_map_copy( lhs.getAsIslMap() ), isl_union_map_copy( rhs.getAsIslMap() )
	);
	return MapPtr<IslContext>(ctx, isl_union_map_get_dim(map), map);
}

template <>
MapPtr<IslContext> 
map_intersect(IslContext& ctx, const Map<IslContext>& lhs, const Map<IslContext>& rhs) {
	isl_union_map* map = isl_union_map_intersect(
			isl_union_map_copy( lhs.getAsIslMap() ), isl_union_map_copy( rhs.getAsIslMap() )
	);
	return MapPtr<IslContext>(ctx, isl_union_map_get_dim(map), map);
}

template <>
MapPtr<IslContext> 
map_intersect_domain(IslContext& ctx, const Map<IslContext>& lhs, const Set<IslContext>& dom) {
	isl_union_map* map = isl_union_map_intersect_domain( 
			isl_union_map_copy(lhs.getAsIslMap()), isl_union_set_copy(dom.getAsIslSet()) 
		);
	return MapPtr<IslContext>(ctx, isl_union_map_get_dim(map), map);
}

//==== Dependence Resolution ======================================================================

template <>
DependenceInfo<IslContext> buildDependencies( 
		IslContext&				ctx,
		const Set<IslContext>& 	domain, 
		const Map<IslContext>& 	schedule, 
		const Map<IslContext>& 	sinks, 
		const Map<IslContext>& 	mustSources,
		const Map<IslContext>& 	maySources
) {
	MapPtr<IslContext>&& schedDom = map_intersect_domain(ctx, schedule, domain);
	MapPtr<IslContext>&& sinksDom = map_intersect_domain(ctx, sinks, domain);

	MapPtr<IslContext>&& mustSourcesDom = map_intersect_domain(ctx, mustSources, domain);
	MapPtr<IslContext>&& maySourcesDom = map_intersect_domain(ctx, maySources, domain);

	isl_union_map *must_dep = NULL, *may_dep = NULL, *must_no_source = NULL, *may_no_source = NULL;

	isl_union_map_compute_flow(
			isl_union_map_copy(sinksDom->getAsIslMap()),
			isl_union_map_copy(mustSourcesDom->getAsIslMap()),
			maySourcesDom ? isl_union_map_copy(maySourcesDom->getAsIslMap()) : NULL,
			isl_union_map_copy(schedDom->getAsIslMap()),
			&must_dep,
			&may_dep,
			&must_no_source,
			&may_no_source
		);	
	
	return DependenceInfo<IslContext>( 
			MapPtr<IslContext>(ctx, isl_union_map_get_dim(must_dep), must_dep ),
			MapPtr<IslContext>(ctx, isl_union_map_get_dim(may_dep), may_dep ),
			MapPtr<IslContext>(ctx, isl_union_map_get_dim(must_no_source), must_no_source ),
			MapPtr<IslContext>(ctx, isl_union_map_get_dim(may_no_source), may_no_source ) );
}

template <>
std::ostream& DependenceInfo<IslContext>::printTo(std::ostream& out) const {
	mustDep->simplify();
	out << std::endl << "* MUST dependencies: " << std::endl;
	mustDep->printTo(out);
	out << "@ Deltas: " << std::endl;
	mustDep->deltas()->printTo(out);

	mayDep->simplify();
	out << std::endl << "* MAY dependencies: " << std::endl;
	mayDep->printTo(out);
	//out << "MUST NO SOURCE dependencies: " << std::endl;
	//mustNoSource->printTo(out);
	//out << "MAY NO SOURCE dependencies: " << std::endl;
	//mayNoSource->printTo(out);
	return out << std::endl;
}

//==== Compute the cardinality of Sets ============================================================

core::ExpressionPtr Set<IslContext>::getCard() const {
	//isl_union_pw_qpolynomial* pw_qpoly = isl_union_set_card( isl_union_set_copy(set) );

	//isl_printer* printer = isl_printer_to_str( ctx.getRawContext() );
	//isl_printer_print_union_pw_qpolynomial(printer, pw_qpoly);

	//char* str = isl_printer_get_str(printer);
	//std::cout << str << std::endl << std::endl;
	//free(str); // free the allocated string by the library
	//isl_printer_free(printer);
	//isl_union_pw_qpolynomial_free(pw_qpoly);
}

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 
