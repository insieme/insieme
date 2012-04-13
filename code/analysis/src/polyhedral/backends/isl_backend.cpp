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
#include "isl/aff.h"

#include "barvinok/isl.h"

namespace insieme { namespace analysis { namespace polyhedral {

namespace {

using namespace insieme::analysis::polyhedral;

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
struct ISLConstraintConverterVisitor : public utils::RecConstraintVisitor<AffineFunction, isl_set*> {

	isl_ctx* ctx;
	isl_space* dim;
	
	ISLConstraintConverterVisitor(isl_ctx* ctx, isl_space* dim) : ctx(ctx), dim(dim) { }

	isl_set* visitRawConstraint(const RawAffineConstraint& rcc) { 
		// std::cout << "Before" << rcc.getConstraint() << std::endl;
		const AffineConstraint& c = rcc.getConstraint();
		if ( isNormalized(c) ) {
			isl_basic_set* bset = setFromConstraint(ctx, dim, c);
			return isl_set_from_basic_set( bset );
		}
		return visit( normalize(c) );
	}

	isl_set* visitNegConstraint(const NegAffineConstraint& ucc) {
		isl_set* sub_set = visit( ucc.getSubConstraint() );

		// in curr_set we have the set coming from the sub constraint, we have to negate it 
		isl_basic_set* universe = isl_basic_set_universe( isl_space_copy(dim) );

		return isl_set_subtract( isl_set_from_basic_set(universe), sub_set );
	}
	
	isl_set* visitBinConstraint(const BinAffineConstraint& bcc) {
		isl_set* lhs = visit(bcc.getLHS());
		isl_set* rhs = visit(bcc.getRHS());
		return bcc.isConjunction() ? isl_set_intersect(lhs, rhs) : isl_set_union(lhs, rhs);
	}
};

template <class IterT>
void setVariableName(isl_ctx *ctx, isl_space*& space, const isl_dim_type& type, IterT const& begin, IterT const& end) {
	for(IterT it = begin; it != end; ++it) {
		assert(dynamic_cast<const Expr*>(&*it) != NULL && "Element of not Variable type");

		// Retrieve the expression associated to this dimension
		const Expr& var = static_cast<const Expr&>(*it);
		std::ostringstream ss;
		ss << var;

		isl_id* id = isl_id_alloc(ctx, ss.str().c_str(), const_cast<core::Expression*>( &(*var.getExpr())) );
		space = isl_space_set_dim_id(space, type, std::distance(begin, it), id);
	}
}

// Utility which converts and isl_space to an IterationVector. They both represent the same concept. The main 
// issue of the translation is on retrieving the pointer to the variable to which an isl paramer is refering 
// to from the user data pointer which ISL provides. 
void visit_space(isl_space* space, core::NodeManager& mgr, IterationVector& iterVec) {
	unsigned iter_num = isl_space_dim( space, isl_dim_set );
	unsigned param_num = isl_space_dim( space, isl_dim_param );

	auto&& extract_ir_expr = [&](unsigned num, const isl_dim_type& type) -> core::ExpressionPtr {
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
		assert(false);
		// We need to create a new Variable to represent this specific iterator/parameter
		return core::IRBuilder(mgr).variable( mgr.getLangBasic().getInt4() );
	};

	for (unsigned i = 0; i < iter_num; ++i) {
		size_t pos = iterVec.add( 
			Iterator(
				core::static_pointer_cast<const core::Variable>(extract_ir_expr(i, isl_dim_set))
			));

		LOG(DEBUG) << core::static_pointer_cast<const core::Variable>(extract_ir_expr(i, isl_dim_set)) << pos << " " << i;
		assert(pos == i);
	}
	
	for (unsigned i = 0; i < param_num; ++i) {
		size_t pos = iterVec.add( Parameter(extract_ir_expr(i, isl_dim_param)) );
		assert(pos-iter_num == i);
	}
	
	return;
}

} // end anonynous namespace




/**********************************************************************************************
 * IslObj: 
 * 	contains the implementation code of IslObjs.
 *
 *********************************************************************************************/

IterationVector IslObj::getIterationVector(core::NodeManager& mgr) const {
	IterationVector ret;
	visit_space(space, mgr, ret);
	return ret;
}


/**********************************************************************************************
 * IslSet: 
 * 	contains the implementation code of IslSets.
 *
 *********************************************************************************************/

IslSet::~IslSet() { 
	isl_union_set_free(set);
}

IslSet::IslSet(IslCtx& ctx, const std::string& set_str) : 
	IslObj(ctx, NULL), set(isl_union_set_read_from_str(ctx.getRawContext(), set_str.c_str())) 
{
	assert( set && "Error parsing input string as a set" );
	space = isl_union_set_get_space( set );
}

IslSet::IslSet(IslCtx& ctx, const IterationDomain& domain, const TupleName& tuple) : IslObj(ctx, NULL) { 

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
	
	if (!tuple.second.empty()) {
		ctx.insertTuple( tuple );
		// Set the name of the tuple 
		space = isl_space_set_tuple_name(space, isl_dim_set, tuple.second.c_str());
	}

	if ( domain.empty() ) {
		set = isl_union_set_from_set(isl_set_empty( isl_space_copy(space) ));
		return;
	} 

	isl_set* cset = NULL;
	if ( domain.universe() ) {
		cset = isl_set_universe( isl_space_copy(space) );
	} else {
		assert( domain.getConstraint() && "Constraints for this iteration domain cannot be empty" );
		// If a non empty constraint is provided, then add it to the universe set 
		ISLConstraintConverterVisitor ccv(ctx.getRawContext(), space);
		cset = ccv.visit(domain.getConstraint());
	}
	
	assert(cset && "ISL set turned to be invalid");

	// Eliminats existential quantitified variables 
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

	if (!tuple.second.empty()) {
		cset = isl_set_set_tuple_name(cset, tuple.second.c_str());
	}
	
	space = isl_set_get_space( cset );
	set = isl_union_set_from_set( cset );
	simplify();
}

bool IslSet::operator==(const IslSet& other) const { 
	return isl_union_set_is_equal( set, other.set );
}

bool IslSet::empty() const { return isl_union_set_is_empty(set);	}

void IslSet::simplify() {
	set = isl_union_set_coalesce( set );
	set = isl_union_set_detect_equalities( set );
}

namespace {

	// Utility function used to print to a stream the ISL internal representation of a set
	void print(std::ostream& out, isl_ctx* ctx, isl_union_set* set) {
		isl_printer* printer = isl_printer_to_str(ctx);
		isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
		isl_printer_set_indent(printer, 1);
		isl_printer_print_union_set(printer, set);
		isl_printer_flush(printer);
		char* str = isl_printer_get_str(printer);
		out << std::string(str);
		free(str); // free the allocated string by the library
		isl_printer_free(printer);
	}

} // end anonymous namespace 

std::ostream& IslSet::printTo(std::ostream& out) const {
	print(out, ctx.getRawContext(), set); 
	return out;
}

SetPtr<ISL> operator+(IslSet& lhs, const IslSet& rhs) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_set* set = isl_union_set_union(lhs.getIslObj(), rhs.getIslObj());
	return SetPtr<ISL>(ctx, set);
}

SetPtr<ISL> operator*(IslSet& lhs, const IslSet& rhs) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_set* set = isl_union_set_intersect(lhs.getIslObj(), rhs.getIslObj());
	return SetPtr<ISL>(ctx, set);
}

// Convertion of an IslSet back to  IR Constraints 
namespace {

struct UserData {
	core::NodeManager& 	mgr;
	IterationVector& 	iterVec;
	AffineConstraintPtr ret;

	UserData(core::NodeManager& mgr, IterationVector& iterVec): 
		mgr(mgr), iterVec(iterVec) { }

	UserData(const UserData& other) : mgr(other.mgr), iterVec(other.iterVec) {}
};

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
	
	assert(user);
	UserData& data = *reinterpret_cast<UserData*>( user );
	IterationVector& iterVec = data.iterVec;
	core::NodeManager& mgr = data.mgr;
	
	// Add the iterators/parameters present inside this space to the itervec
	visit_space(space, mgr, iterVec);

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


AffineConstraintPtr IslSet::toConstraint(core::NodeManager& mgr, IterationVector& iterVec) const {
	
	UserData data(mgr, iterVec);
	isl_union_set_foreach_set(set, visit_set, &data);

	return data.ret;	
}

PiecewisePtr<ISL> IslSet::getCard() const {
	return PiecewisePtr<ISL>(ctx, isl_union_set_card( isl_union_set_copy(set) ) );
}



/**********************************************************************************************
 * IslMap: 
 * 	contains the implementation code of IslMaps.
 *
 *********************************************************************************************/

IslMap::IslMap(IslCtx& ctx, const std::string& map_str) : IslObj(ctx, NULL) {
	map = isl_union_map_read_from_str(ctx.getRawContext(), map_str.c_str());
	assert(map && "Error while reading map from input string");
	space = isl_union_map_get_space(map);
}

IslMap::IslMap(IslCtx& 				ctx, 
			   const AffineSystem& 	affSys, 
			   const TupleName&	 	in_tuple, 
			   const TupleName& 	out_tuple 
			  ) : IslObj(ctx)
{
	const IterationVector& iterVec = affSys.getIterationVector();

	// Build the dim object
	space = isl_space_alloc( ctx.getRawContext(), iterVec.getParameterNum(), iterVec.getIteratorNum(), affSys.size());

	// Set the names for the iterators of this dim
	setVariableName(ctx.getRawContext(), space, isl_dim_in, iterVec.iter_begin(), iterVec.iter_end());

	// Set the names for the parameters of this dim
	setVariableName(ctx.getRawContext(), space, isl_dim_param, iterVec.param_begin(), iterVec.param_end());

	// Set the input tuple name if specified
	if ( !in_tuple.second.empty() ) {
		ctx.insertTuple( in_tuple );
		space = isl_space_set_tuple_name(space, isl_dim_in, in_tuple.second.c_str());
	}

	if ( !out_tuple.second.empty() ) {
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

	// Eliminats existential quantitified variables 
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

	if ( !in_tuple.second.empty() ) {
		bmap = isl_basic_map_set_tuple_name( bmap, isl_dim_in, in_tuple.second.c_str()); 
	}

	if ( !out_tuple.second.empty() ) {
		bmap = isl_basic_map_set_tuple_name( bmap, isl_dim_out, out_tuple.second.c_str());
	}

	isl_space_free(space);
	space = isl_basic_map_get_space( bmap );

	// convert the basic map into a map
	map = isl_union_map_from_map(isl_map_from_basic_map(bmap));
	simplify();
}

	// Apply operator 
MapPtr<> IslMap::operator()(const IslMap& other) const {
	return MapPtr<>(ctx, isl_union_map_apply_range(isl_union_map_copy(map), isl_union_map_copy(other.map)));
}


PiecewisePtr<ISL> IslMap::getCard() const {
	return PiecewisePtr<ISL>(ctx, isl_union_map_card( isl_union_map_copy(map) ) );
}

namespace {
	// Utility function used to print to an output stream the ISL internal representation of maps (or relations)
	void print(std::ostream& out, isl_ctx* ctx, isl_union_map* map) {
		isl_printer* printer = isl_printer_to_str(ctx);
		isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
		isl_printer_set_indent(printer, 1);
		isl_printer_print_union_map(printer, map);
		isl_printer_flush(printer);
		char* str =  isl_printer_get_str(printer);
		out << std::string(str);
		free(str);
		isl_printer_free(printer);
	}
} // end anonymous namespace 

std::ostream& IslMap::printTo(std::ostream& out) const {
	print(out, ctx.getRawContext(), map); 
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

MapPtr<ISL> operator+(IslMap& lhs, const IslMap& rhs) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_map* map = isl_union_map_union( lhs.getIslObj(), rhs.getIslObj() );
	return MapPtr<ISL>(ctx, map);
}

MapPtr<ISL> operator*(IslMap& lhs, const IslMap& rhs) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_map* map = isl_union_map_intersect( lhs.getIslObj(), rhs.getIslObj() );
	return MapPtr<ISL>(ctx, map);
}

MapPtr<ISL> operator*(IslMap& lhs, const IslSet& dom) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_map* map = isl_union_map_intersect_domain( lhs.getIslObj(), dom.getIslObj() );
	return MapPtr<ISL>(ctx, map);
}

SetPtr<ISL> range(IslMap& map) {
	isl_union_set* set = isl_union_map_range(map.getIslObj());
	return SetPtr<ISL>(map.getCtx(), set);
}

MapPtr<ISL> range_map(IslMap& map) {
	isl_union_map* rmap = isl_union_map_range_map(map.getIslObj());
	return MapPtr<ISL>(map.getCtx(), rmap);
}

SetPtr<ISL> domain(IslMap& map) {
	isl_union_set* set = isl_union_map_domain(map.getIslObj());
	return SetPtr<ISL>(map.getCtx(), set);
}

MapPtr<ISL> reverse(IslMap& map) {
	isl_union_map* invMap = isl_union_map_reverse(map.getIslObj());
	return MapPtr<ISL>(map.getCtx(), invMap);
}

MapPtr<ISL> domain_map(IslMap& map) {
	isl_union_map* domainMap = isl_union_map_domain_map(map.getIslObj());
	return MapPtr<ISL>(map.getCtx(), domainMap);
}




/**********************************************************************************************
 * Dependency Resolution:
 * 	contains the code which is utilized to perform dependency analysis in the polyhedral model
 *
 *********************************************************************************************/

DependenceInfo<ISL> buildDependencies( 
		IslCtx&		ctx,
		IslSet& 	domain, 
		IslMap& 	schedule, 
		IslMap& 	sinks, 
		IslMap& 	mustSources,
		IslMap& 	maySources
) {
	MapPtr<ISL>&& schedDom = schedule * domain;
	MapPtr<ISL>&& sinksDom = sinks * domain;

	MapPtr<ISL>&& mustSourcesDom = mustSources * domain;
	MapPtr<ISL>&& maySourcesDom = maySources * domain;

	isl_union_map *must_dep = NULL, *may_dep = NULL, *must_no_source = NULL, *may_no_source = NULL;

	isl_union_map_compute_flow(
			sinksDom->getIslObj(),
			mustSourcesDom->getIslObj(),
			maySourcesDom ? maySourcesDom->getIslObj() : NULL,
			schedDom->getIslObj(),
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

// FIXME: what the hell is this?
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




/**********************************************************************************************
 * IslPiecewise: 
 * 	contains the implementation code of IslPiecewise.
 *
 *********************************************************************************************/

namespace {

namespace arith = insieme::core::arithmetic;

typedef std::tuple<
	core::NodeManager&, 
	IterationVector&, 
	arith::Formula, 
	utils::CombinerPtr<arith::Formula>
> PieceData;

typedef std::tuple<core::NodeManager&, IterationVector&, arith::Formula> TermData;

int visit_isl_term(isl_term *term, void *user) {

	isl_int intVal;
	isl_int_init(intVal);
	
	TermData& data = *reinterpret_cast<TermData*>(user);

	IterationVector& iv = std::get<1>(data);
	// Conversion of ISL int INT4
	auto&& isl_int_to_c_int = [ ] (const isl_int& val) {
		char* str = isl_int_get_str(val);
		std::string strVal( str );
		free(str);
		return utils::numeric_cast<int>( strVal );
	};

	isl_term_get_num(term, &intVal);
	int numerator = isl_int_to_c_int(intVal);

	isl_term_get_den(term, &intVal);
	int denominator = isl_int_to_c_int(intVal);
	
	arith::Formula ret(arith::Rational(numerator, denominator));

	for(size_t idx = 0, end = isl_term_dim(term, isl_dim_param); idx != end; ++idx) {
		int exp = isl_term_get_exp(term, isl_dim_param, idx);
		if (exp == 0) { continue; }

		core::ExpressionPtr expr = static_cast<const Expr&>(iv[idx+iv.getIteratorNum()]).getExpr();
		if (expr->getType()->getNodeType() == core::NT_RefType) {
			expr = core::IRBuilder(std::get<0>(data)).deref(expr);
		}
		ret = ret * arith::Product(expr, exp);
	}

	for(size_t idx = 0, end =  isl_term_dim(term, isl_dim_div); idx < end; ++idx) {
		int exp = isl_term_get_exp(term, isl_dim_div, idx);
		if(exp == 0) { continue; }

		isl_aff* aff = isl_term_get_div(term, idx);
		
		isl_aff_get_constant(aff, &intVal);
		int constant = isl_int_to_c_int(intVal);

		arith::Formula tmp = constant;

		isl_aff_get_denominator(aff, &intVal);
		int denominator = isl_int_to_c_int(intVal);

		for(size_t idx = 0, end = isl_aff_dim(aff, isl_dim_param); idx < end; ++idx) {
			isl_aff_get_coefficient(aff, isl_dim_param, idx, &intVal);
			int coeff = isl_int_to_c_int(intVal);
			// if the coefficient is 0, continue
			if (coeff == 0) { continue; }
			
			core::ExpressionPtr expr = static_cast<const Expr&>(iv[idx+iv.getIteratorNum()]).getExpr();
			// Retrive the parameter associated to this position 
			if (expr->getType()->getNodeType() == core::NT_RefType) {
				expr = core::IRBuilder(std::get<0>(data)).deref(expr);
			}
			tmp = tmp + arith::Value(expr) * coeff;
		}
	
		// free the affine expression
		isl_aff_free(aff);

		// Apply the denominator. FIXME: The operation should contain a floor which is not supported by the Formulas
		tmp = tmp / denominator;

		// because we cannot perform the exponentiation of a formula, we multiply with itself for N times 
		while(--exp != 0) { tmp = tmp * tmp; };
	
		ret = ret * tmp;
	}

	std::get<2>(data) = std::get<2>(data) + ret;
	
	isl_int_clear(intVal);
	isl_term_free(term);
	return 0;
}

typedef std::tuple<
	isl_ctx*,
	core::NodeManager&, 
	utils::Piecewise<arith::Formula>::Pieces
> PiecewiseData;

void print(IslCtx& ctx, std::ostream& out, isl_qpolynomial* qp) {
	isl_printer* printer = isl_printer_to_str( ctx.getRawContext() );
	isl_printer_print_qpolynomial(printer, qp);
	char* str = isl_printer_get_str(printer);
	out << str;
	free(str); // free the allocated string by the library
	isl_printer_free(printer);
}

int visit_isl_pw_qpolynomial_piece(isl_set *set, isl_qpolynomial *qp, void *user) {
	
	IterationVector iterVec;
	
	PiecewiseData& pwdata = *reinterpret_cast<PiecewiseData*>(user);
	core::NodeManager& mgr = std::get<1>(pwdata);

	// Create a temporary data object to hold the information collected by the sub visit methods
	UserData data(mgr, iterVec);
	visit_set(set, &data);

	isl_space* space = isl_qpolynomial_get_domain_space(qp);
	visit_space(space, mgr, iterVec);

	assert(!isl_qpolynomial_is_infty(qp) && "Infinity cardinality is not supported");

	TermData td(mgr, iterVec, arith::Formula());
	isl_qpolynomial_foreach_term(qp, visit_isl_term, &td);
	arith::Formula& ret = std::get<2>(td);

	utils::Piecewise<arith::Formula>::PredicatePtr pred = 
		makeCombiner( utils::Constraint<arith::Formula>( arith::Formula(), utils::ConstraintType::EQ ) );
	// the default constraint is the equality 0  == 0 which is used for pieces which are always
	// defined

	if (data.ret) {
		// This is a picewise
		pred = utils::castTo<AffineFunction, arith::Formula>(data.ret);
	} 
	
	std::get<2>(pwdata).push_back( utils::Piecewise<arith::Formula>::Piece(pred, ret) );
	
	isl_space_free(space);
	isl_qpolynomial_free(qp);
	return 0;
}

int visit_pw_qpolynomial(isl_pw_qpolynomial *pwqp, void *user) {
	int ret = isl_pw_qpolynomial_foreach_lifted_piece(pwqp, visit_isl_pw_qpolynomial_piece, user);
	isl_pw_qpolynomial_free(pwqp);
	return ret;
}

} // end anonymous namespace 

//==== Piecewise ==================================================================================

IslPiecewise::IslPiecewise(IslCtx& ctx) : IslObj(ctx, isl_space_params_alloc( ctx.getRawContext(), 0 )) {
	pw = isl_union_pw_qpolynomial_zero( isl_space_copy(space) );
}

insieme::utils::Piecewise<insieme::core::arithmetic::Formula> IslPiecewise::toPiecewise(core::NodeManager& mgr) const {
	PiecewiseData data(ctx.getRawContext(), mgr, utils::Piecewise<arith::Formula>::Pieces());
	isl_union_pw_qpolynomial_foreach_pw_qpolynomial( pw, visit_pw_qpolynomial, &data );

	return utils::Piecewise<arith::Formula>( std::get<2>(data) );
}

namespace {

	void print(std::ostream& out, isl_ctx* ctx, isl_union_pw_qpolynomial* pw) {
		// Print the polynomial just for debugging purposes
		isl_printer* printer = isl_printer_to_str( ctx );
		isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
		isl_printer_set_indent(printer, 1);
		isl_printer_print_union_pw_qpolynomial(printer, pw);
		char* str = isl_printer_get_str(printer);
		out << std::string(str);
		free(str); // free the allocated string by the library
		isl_printer_free(printer);
	}

} // end anonymous namespace 

std::ostream& IslPiecewise::printTo(std::ostream& out) const {
	print(out, ctx.getRawContext(), pw); 
	return out;
}

void IslPiecewise::simplify() {
	pw = isl_union_pw_qpolynomial_coalesce(pw);
}

PiecewisePtr<ISL> operator+(IslPiecewise& lhs, IslPiecewise& rhs) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_pw_qpolynomial* ret = isl_union_pw_qpolynomial_add(lhs.getIslObj(), rhs.getIslObj());
	return PiecewisePtr<ISL>(ctx, ret);
}

PiecewisePtr<ISL> operator*(IslPiecewise& lhs, const IslSet& dom) {
	IslCtx& ctx = lhs.getCtx();
	isl_union_pw_qpolynomial* pw = isl_union_pw_qpolynomial_intersect_domain( lhs.getIslObj(), dom.getIslObj() );
	return PiecewisePtr<ISL>(ctx, pw);
}


namespace {

typedef std::vector<double> FoldUserData;

void print(IslCtx& ctx, std::ostream& out, isl_union_pw_qpolynomial_fold* fold) {	
	isl_printer* printer = isl_printer_to_str(ctx.getRawContext());
	isl_printer_set_output_format(printer, ISL_FORMAT_ISL);
	isl_printer_set_indent(printer, 1);
	isl_printer_print_union_pw_qpolynomial_fold(printer, fold);
	isl_printer_flush(printer);
	char* str = isl_printer_get_str(printer);
	out << std::string(str);
	free(str); // free the allocated string by the library
	isl_printer_free(printer);
}

int visit_qpolynomial(isl_qpolynomial* p, void* user) {
	isl_int num, den;
	isl_int_init(num);
	isl_int_init(den);

	// Conversion of ISL int INT4
	auto&& isl_int_to_c_int = [ ] (const isl_int& val) {
		char* str = isl_int_get_str(val);
		std::string strVal( str );
		free(str);
		return utils::numeric_cast<int>( strVal );
	};
	
	FoldUserData& data = *reinterpret_cast<FoldUserData*>(user);
	//int ret = isl_qpolynomial_is_cst(p, &num, &den);

	data.push_back( isl_int_to_c_int(num) / isl_int_to_c_int(den) );
	
	isl_int_clear(num);
	isl_int_clear(den);
	isl_qpolynomial_free(p);
	return 0;
}

int visit_fold_piece(isl_set* dom, isl_qpolynomial_fold* fold, void* user) {
	int ret = isl_qpolynomial_fold_foreach_qpolynomial(fold, visit_qpolynomial, user);
	// FIXME: Visit the domain in order to perform a complete conversion
	isl_set_free(dom);
	isl_qpolynomial_fold_free(fold);
	return ret;
}

// visit qpolynomial_fold
int visit_pw_qpolynomial_fold(isl_pw_qpolynomial_fold *pwf, void *user) {
	int ret = isl_pw_qpolynomial_fold_foreach_piece(pwf, visit_fold_piece, user);
	isl_pw_qpolynomial_fold_free(pwf);
	return ret;
}

} // end anonymous namespace 

double IslPiecewise::upperBound() const {
	
	int tight;
	isl_union_pw_qpolynomial_fold* fold = isl_union_pw_qpolynomial_bound(getIslObj(), isl_fold_max, &tight);

	//print(ctx, std::cout, fold);
	//std::cout << std::endl;

	FoldUserData data;
	// Inspect the pw_qpolynomial_fold
	int ret = isl_union_pw_qpolynomial_fold_foreach_pw_qpolynomial_fold(fold, visit_pw_qpolynomial_fold, &data);

	isl_union_pw_qpolynomial_fold_free(fold);

	if (data.empty()) { return 0; }

	assert(ret==0 && data.size() == 1 && "Fold contains more than 1 polynomial. NOT SUPPORTED!");
	return data.front();
}

} } } // end insieme::analysis::polyhedral
