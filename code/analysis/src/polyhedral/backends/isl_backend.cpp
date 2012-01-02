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
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/logging.h"

#include "isl/space.h"
#include "isl/set.h"
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

isl_constraint* convertConstraint ( 
		isl_ctx*					islCtx, 
		isl_space* 					space, 
		const AffineConstraint& 	constraint, 
		const isl_dim_type& 		type ) 
{
	isl_constraint* islCons = NULL;

	isl_int intVal;
	isl_int_init(intVal);
	
	islCons = (constraint.getType() == ConstraintType::EQ) ? 
				isl_equality_alloc(isl_local_space_from_space( isl_space_copy(space) )) : 
				isl_inequality_alloc(isl_local_space_from_space( isl_space_copy(space) ));
	
	const AffineFunction& af = constraint.getFunction();
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

bool isNormalized(const AffineConstraint& c) {
	return c.getType() == ConstraintType::EQ || c.getType() == ConstraintType::GE;
}

isl_basic_set* setFromConstraint(isl_ctx* islCtx, isl_space* dim, const AffineConstraint& c) {

	// check whether the constraint is properly normalize 
	assert(isNormalized(c) && "Constraint not normlized");

	// Create an ISL basic_set 
	isl_basic_set* bset = isl_basic_set_universe( isl_space_copy(dim) );

	// Create the ISL constraint 
	isl_constraint* cons = convertConstraint( islCtx, dim, c, isl_dim_set);
	
	// Add the constraint to the basic_set
	return isl_basic_set_add_constraint( bset, cons );
}

// Visits the Constraint combiner and builds the corresponding ISL set 
struct ISLConstraintConverterVisitor : public utils::RecConstraintVisitor<AffineFunction> {

	isl_ctx* ctx;
	isl_space* dim;
	
	isl_set* curr_set;

	ISLConstraintConverterVisitor(isl_ctx* ctx, isl_space* dim) : ctx(ctx), dim(dim) { }

	void visit(const RawAffineConstraint& rcc) { 
		// std::cout << "Before" << rcc.getConstraint() << std::endl;
		const AffineConstraint& c = rcc.getConstraint();
		if ( isNormalized(c) ) {
			isl_basic_set* bset = setFromConstraint(ctx, dim, c);
			curr_set = isl_set_from_basic_set( bset );
			return;
		}
		normalize(c)->accept(*this);
	}

	void visit(const NegatedAffineConstraint& ucc) {
		RecConstraintVisitor::visit( ucc.getSubConstraint() );
		// in curr_set we have the set coming from the sub constraint, we have to negate it 
		isl_basic_set* universe = isl_basic_set_universe( isl_space_copy(dim) );

		curr_set = isl_set_subtract( isl_set_from_basic_set(universe), curr_set );
	}
	
	void visit(const BinaryAffineConstraint& bcc) {
		bcc.getLHS()->accept(*this);
		isl_set* lhs = curr_set;

		bcc.getRHS()->accept(*this);
		isl_set* rhs = curr_set;

		curr_set = bcc.isConjunction() ? isl_set_intersect(lhs, rhs) : isl_set_union(lhs, rhs);
	}

	isl_set* getResult() const { return curr_set; }

};

template <class IterT>
void setVariableName(isl_ctx *ctx, isl_space*& space, const isl_dim_type& type, IterT const& begin, IterT const& end) {
	for(IterT it = begin; it != end; ++it) {
		assert(dynamic_cast<const Expr*>(&*it) != NULL && "Element of not Variable type");

		// Retrieve the expression associated to this dimension
		const poly::Expr& var = static_cast<const Expr&>(*it);
		std::ostringstream ss;
		ss << var;

		isl_id* id = isl_id_alloc(ctx, ss.str().c_str(), const_cast<core::Expression*>( &(*var.getExpr())) );
		space = isl_space_set_dim_id(space, type, std::distance(begin, it), id);
	}
}
} // end anonynous namespace

//==== Set ====================================================================================

IslSet::~IslSet() { 
	isl_space_free(space);
	isl_union_set_free(set);
}

IslSet::IslSet(IslCtx& ctx, const IterationDomain& domain, const TupleName& tuple) : ctx(ctx) { 

	const IterationVector& iterVec = domain.getIterationVector();

	// Build the space object
	if ( iterVec.getIteratorNum() != 0 ) {
		space = isl_space_set_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum() );
	} else {
		space = isl_space_params_alloc( ctx.getRawContext(), iterVec.getParameterNum() );
	}

	// Set the names for the iterators of this dim
	setVariableName(ctx.getRawContext(), space, isl_dim_set, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(ctx.getRawContext(), space, isl_dim_param, iterVec.param_begin(), iterVec.param_end());
	
	if (tuple.first) {
		ctx.insertTuple( tuple );
		// Set the name of the tuple 
		space = isl_space_set_tuple_name(space, isl_dim_set, tuple.second.c_str());
	}

	if ( domain.empty() ) {
		set = isl_union_set_from_set(isl_set_empty( isl_space_copy(space) ));
		return;
	} 

	isl_set* cset;
	
	if ( domain.isUniverse() ) {
		cset = isl_set_universe( isl_space_copy(space) );
	} else {
		assert( domain.getConstraint() && "Constraints for this iteration domain cannot be empty" );
		// If a non empty constraint is provided, then add it to the universe set 
		ISLConstraintConverterVisitor ccv(ctx.getRawContext(), space);
		domain.getConstraint()->accept(ccv);

		cset = ccv.getResult();
	}
	
	assert(cset && "ISL set turned to be invalid");

	size_t pos = 0;
	std::for_each ( iterVec.iter_begin(), iterVec.iter_end(),
		[&]( const Iterator& iter ) {
			// peel out this dimension by projecting it 
			if ( iter.isExistential() ) { 
				cset = isl_set_project_out( cset, isl_dim_set, pos, 1); 
				return;
			} 
			pos++;
		}
	);
	assert(cset && "After projection set turn to be invalid");
	isl_space_free(space);

	if (tuple.first) {
		cset = isl_set_set_tuple_name(cset, tuple.second.c_str());
	}
	
	space = isl_set_get_space( cset );
	set = isl_union_set_from_set( cset );
}

bool IslSet::operator==(const IslSet& other) const {
	return isl_union_set_is_equal( set, other.set );
}

bool IslSet::empty() const { return isl_union_set_is_empty(set);	}

void IslSet::simplify() {
	set = isl_union_set_coalesce( set );
	set = isl_union_set_detect_equalities( set );
}

std::ostream& IslSet::printTo(std::ostream& out) const {
	printIslSet(out, ctx.getRawContext(), set); 
	return out;
}

namespace {

struct UserData {
	core::NodeManager& 	mgr;
	IterationVector& 	iterVec;
	AffineConstraintPtr ret;

	UserData(core::NodeManager& mgr, IterationVector& iterVec): 
		mgr(mgr), iterVec(iterVec) { }

	UserData(const UserData& other) : mgr(other.mgr), iterVec(other.iterVec) {}
};

template <class T>
void set_elem_coeff ( const isl_dim_type& dim, const T& elem, AffineFunction& func) {

}

int visit_constraint(isl_constraint* cons, void* user) {
	assert(user && "Invalid User data");

	UserData& data = *reinterpret_cast<UserData*>( user );
	IterationVector& iv = data.iterVec;

	// Conversion of ISL int INT4
	auto&& isl_int_to_c_int = [ ] (const isl_int& val) {
		char* str = isl_int_get_str(val);
		std::string strVal( str );
		free(str);
		return utils::numeric_cast<int>( strVal );
	};

	AffineFunction func(data.iterVec);

	auto set_elem_coeff = [&](const isl_dim_type& type, const Element& elem) -> void {
		isl_int intVal;
		isl_int_init(intVal); 

		unsigned idx = iv.getIdx(elem);
		if (elem.getType() == Element::PARAM) {
			assert (idx >= iv.getIteratorNum());
			idx -= iv.getIteratorNum();
		}
		isl_constraint_get_coefficient( cons, type, idx, &intVal);
		func.setCoeff( elem, isl_int_to_c_int(intVal) );
		isl_int_clear(intVal);
	};

	isl_int intVal;
	isl_int_init(intVal); 

	// retrieve the constant coefficient 
	isl_constraint_get_constant(cons, &intVal);
	func.setCoeff( Constant(), isl_int_to_c_int(intVal));

	// retrieve the coefficients for the iterators 
	std::for_each(iv.iter_begin(), iv.iter_end(), std::bind(set_elem_coeff, isl_dim_set, std::placeholders::_1));
	
	// retrieve the coefficients for the parameters
	std::for_each(iv.param_begin(), iv.param_end(), std::bind(set_elem_coeff, isl_dim_param, std::placeholders::_1));

	// retrieve the type of inequality
	AffineConstraint affCons( func, isl_constraint_is_equality( cons ) ? ConstraintType::EQ : ConstraintType::GE );

	data.ret = !data.ret ? makeCombiner(affCons) : data.ret and affCons;
	
	isl_constraint_free( cons );
	isl_int_clear(intVal);
	return 0;
}

int visit_basic_set(isl_basic_set* bset, void* user) {
	isl_space* space = isl_basic_set_get_space( bset);
	assert(space && isl_space_is_set(space) );
	
	unsigned iter_num = isl_space_dim( space, isl_dim_set );
	unsigned param_num = isl_space_dim( space, isl_dim_param );
	
	assert(user);
	UserData& data = *reinterpret_cast<UserData*>( user );
	IterationVector& iterVec = data.iterVec;
	core::NodeManager& mgr = data.mgr;

	auto&& extract_ir_expr = [&](unsigned num, const isl_dim_type& type) {
		// Determine whether this dimension has an isl_id associated 
		if (isl_space_has_dim_id( space, type, num )) {
			isl_id* id = isl_space_get_dim_id( space, type, num);
			assert (id && "ISL Set has no user data associated");
			const core::Expression* expr = reinterpret_cast<const core::Expression*>( isl_id_get_user(id) );
			// Free the ID object
			isl_id_free( id );

			core::ExpressionPtr ir_expr = mgr.lookup(expr);

			assert (ir_expr && "Retrieve of user information within ISL set failed");
			return ir_expr;
		}
		assert(false && "Not yet supportet");
	};

	for (unsigned i = 0; i < iter_num; ++i) 
		iterVec.add( 
			poly::Iterator(
				core::static_pointer_cast<const core::Variable>(extract_ir_expr(i, isl_dim_set))
			));
	
	for (unsigned i = 0; i < param_num; ++i)
		iterVec.add( poly::Parameter(extract_ir_expr(i, isl_dim_param)) );


	UserData tmp(data);
	// Iterate through the constraints 
	isl_basic_set_foreach_constraint(bset, visit_constraint, &tmp);
	data.ret = !data.ret ? tmp.ret : data.ret or tmp.ret;


	isl_basic_set_free(bset);
	isl_space_free(space);
	return 0;
}

int visit_set(isl_set* set, void* user) {
	UserData& data = *reinterpret_cast<UserData*>( user );
	UserData tmp(data);

	isl_set_foreach_basic_set(set, visit_basic_set, &tmp);
	isl_set_free(set);
	data.ret = !data.ret ? tmp.ret : data.ret or tmp.ret;

	return 0;
}

} // end anonymous namespace

poly::AffineConstraintPtr IslSet::toConstraint(core::NodeManager& mgr, poly::IterationVector& iterVec) const {
	
	UserData data(mgr, iterVec);
	printIslSet(std::cout, ctx.getRawContext(), set);
	isl_union_set_foreach_set(set, visit_set, &data);

	return data.ret;	
}

//==== Map ====================================================================================

IslMap::IslMap(IslCtx& 				ctx, 
			   const AffineSystem& 	affSys, 
			   const TupleName&	 	in_tuple, 
			   const TupleName& 	out_tuple 
			  ) : ctx(ctx)
{
	const IterationVector& iterVec = affSys.getIterationVector();

	// Build the dim object
	space = isl_space_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum(), affSys.size());

	// Set the names for the iterators of this dim
	setVariableName(ctx.getRawContext(), space, isl_dim_in, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(ctx.getRawContext(), space, isl_dim_param, iterVec.param_begin(), iterVec.param_end());

	// Set the input tuple name if specified
	if ( in_tuple.first ) {
		ctx.insertTuple( in_tuple );
		space = isl_space_set_tuple_name(space, isl_dim_in, in_tuple.second.c_str());
	}

	if ( out_tuple.first ) {
		ctx.insertTuple( out_tuple ); 
		space = isl_space_set_tuple_name(space, isl_dim_out, out_tuple.second.c_str());
	}
	
	// creates an universe set containing the dimensionatility of the iteration vector
	size_t idx=0;

	if (affSys.size() == 0) {
		// create an empty map
		map = isl_union_map_from_map( isl_map_empty( isl_space_copy(space) ) );
		return;
	}
	isl_basic_map* bmap = isl_basic_map_universe( isl_space_copy(space) );
	for(AffineSystem::const_iterator it=affSys.begin(), end=affSys.end(); it!=end; ++it, ++idx) {
		// std::cout << "SCHED" << std::endl;
		isl_constraint* cons = convertConstraint(ctx.getRawContext(), 
									space, 
									AffineConstraint(*it, ConstraintType::EQ), 
									isl_dim_in
								);
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

	size_t pos = 0;
	std::for_each ( iterVec.iter_begin(), iterVec.iter_end(),
		[&]( const Iterator& iter ) {
			//// peel out this dimension by projecting it 
			if ( iter.isExistential() ) { 
				bmap = isl_basic_map_project_out( bmap, isl_dim_in, pos, 1); 
				return;
			} 
			pos++;
		}
	);

	if ( in_tuple.first ) {
		bmap = isl_basic_map_set_tuple_name( bmap, isl_dim_in, in_tuple.second.c_str()); 
	}

	if ( out_tuple.first ) {
		bmap = isl_basic_map_set_tuple_name( bmap, isl_dim_out, out_tuple.second.c_str());
	}

	isl_space_free(space);
	space = isl_basic_map_get_space( bmap );

	// convert the basic map into a map
	map = isl_union_map_from_map(isl_map_from_basic_map(bmap));
}

std::ostream& IslMap::printTo(std::ostream& out) const {
	printIslMap(out, ctx.getRawContext(), map); 
	return out;
}

void IslMap::simplify() {
	map = isl_union_map_coalesce( map );
	map = isl_union_map_detect_equalities( map );
}

SetPtr<ISL> IslMap::deltas() const {
	isl_union_set* deltas = isl_union_map_deltas( isl_union_map_copy(map) );
	return SetPtr<ISL>(ctx, deltas);
}

MapPtr<ISL> IslMap::deltas_map() const {
	isl_union_map* deltas = isl_union_map_deltas_map( isl_union_map_copy(map) );
	return MapPtr<ISL>(ctx, deltas);
}

bool IslMap::empty() const { 
	return !map || isl_union_map_is_empty(map);	
}

//==== Sets and Maps operations ===================================================================

SetPtr<ISL> set_union(IslCtx& ctx, const IslSet& lhs, const IslSet& rhs) {
	isl_union_set* set = isl_union_set_union(
			isl_union_set_copy( lhs.getAsIslSet() ), isl_union_set_copy( rhs.getAsIslSet() )
	);
	return SetPtr<ISL>(ctx, set);
}

SetPtr<ISL> set_intersect(IslCtx& ctx, const IslSet& lhs, const IslSet& rhs) {
	isl_union_set* set = isl_union_set_intersect(
			isl_union_set_copy( lhs.getAsIslSet() ), isl_union_set_copy( rhs.getAsIslSet() )
	);
	return SetPtr<ISL>(ctx, set);
}

MapPtr<ISL> map_union(IslCtx& ctx, const IslMap& lhs, const IslMap& rhs) {
	isl_union_map* map = isl_union_map_union( 
			isl_union_map_copy( lhs.getAsIslMap() ), isl_union_map_copy( rhs.getAsIslMap() )
	);
	return MapPtr<ISL>(ctx, map);
}

MapPtr<ISL> map_intersect(IslCtx& ctx, const IslMap& lhs, const IslMap& rhs) {
	isl_union_map* map = isl_union_map_intersect(
			isl_union_map_copy( lhs.getAsIslMap() ), isl_union_map_copy( rhs.getAsIslMap() )
	);
	return MapPtr<ISL>(ctx, map);
}

MapPtr<ISL> map_intersect_domain(IslCtx& ctx, const IslMap& lhs, const IslSet& dom) {
	isl_union_map* map = isl_union_map_intersect_domain( 
			isl_union_map_copy(lhs.getAsIslMap()), isl_union_set_copy(dom.getAsIslSet()) 
		);
	return MapPtr<ISL>(ctx, map);
}

//==== Dependence Resolution ======================================================================

DependenceInfo<ISL> buildDependencies( 
		IslCtx&			ctx,
		const IslSet& 	domain, 
		const IslMap& 	schedule, 
		const IslMap& 	sinks, 
		const IslMap& 	mustSources,
		const IslMap& 	maySources
) {
	MapPtr<ISL>&& schedDom = map_intersect_domain(ctx, schedule, domain);
	MapPtr<ISL>&& sinksDom = map_intersect_domain(ctx, sinks, domain);

	MapPtr<ISL>&& mustSourcesDom = map_intersect_domain(ctx, mustSources, domain);
	MapPtr<ISL>&& maySourcesDom = map_intersect_domain(ctx, maySources, domain);

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
	
	return DependenceInfo<ISL>(
			MapPtr<ISL>(ctx, must_dep ),
			MapPtr<ISL>(ctx, may_dep ),
			MapPtr<ISL>(ctx, must_no_source ),
			MapPtr<ISL>(ctx, may_no_source ) 
		);
}

template <>
std::ostream& DependenceInfo<ISL>::printTo(std::ostream& out) const {
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

namespace {

int visit_isl_term(isl_term *term, void *user) {

	isl_int intVal;
	isl_int_init(intVal);

	std::cout << isl_term_dim(term, isl_dim_set) << std::endl; 
	std::cout << isl_term_dim(term, isl_dim_param) << std::endl; 
	
	isl_term_get_num(term, &intVal);

	isl_term_get_den(term, &intVal);


}
	
int visit_isl_pw_qpolynomial_piece(isl_set *set, isl_qpolynomial *qp, void *user) {
	
	IterationVector iterVec;
	UserData data( *reinterpret_cast<core::NodeManager*>(user), iterVec );
	visit_set(set, &data);

	if (!isl_qpolynomial_is_infty(qp))
		isl_qpolynomial_foreach_term(qp, visit_isl_term, user);
	std::cout << *data.ret;
}

int visit_pw_qpolynomial(isl_pw_qpolynomial *pwqp, void *user) {
	return isl_pw_qpolynomial_foreach_lifted_piece(pwqp, visit_isl_pw_qpolynomial_piece, user);
}


} // end anonymous namespace 

void IslSet::getCard(core::NodeManager& mgr) const {
	isl_union_pw_qpolynomial* pw_qpoly = isl_union_set_card( isl_union_set_copy(set) );
	IterationVector iv;
	std::cout << *toConstraint(mgr, iv) << std::endl;

	isl_printer* printer = isl_printer_to_str( ctx.getRawContext() );
	isl_printer_print_union_pw_qpolynomial(printer, pw_qpoly);
	char* str = isl_printer_get_str(printer);
	std::cout << str << std::endl << std::endl;
	free(str); // free the allocated string by the library
	isl_printer_free(printer);

	// isl_union_pw_qpolynomial_foreach_pw_qpolynomial( pw_qpoly, visit_pw_qpolynomial, &mgr );
	
	isl_union_pw_qpolynomial_free(pw_qpoly);
}

} // end poly namespace 
} // end analysis namespace 
} // end insieme namespace 
