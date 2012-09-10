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

#include "insieme/analysis/func_sema.h"

#include "insieme/analysis/defuse_collect.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/func_pipeline.h"

#include "insieme/utils/timer.h"

namespace insieme {
namespace analysis {

using insieme::core::arithmetic::Formula;
using insieme::core::arithmetic::Piecewise;

// Given a concrete call expression, this method returns the argument to which this object is referring
core::ExpressionPtr FunctionArgument::operator()(const core::CallExprPtr& callExpr) const {
	core::LiteralPtr callFunc = core::static_pointer_cast<const core::Literal>(callExpr->getFunctionExpr());
	assert(*funcLit == *callFunc && "CallExpression is not of the same type");
	assert(pos < callExpr->getArguments().size());
	return callExpr->getArgument(pos);
}

namespace {

// filter unwanted references 
RefList filterUnwanted(core::NodeManager& mgr, const RefList& refs) {

	// It may happen that some of the reference we obtain are non-sense because of the restriction of the IR and the way
	// the frontend deals with arrays and pointers. For this reason we filter out known literal which have no semantical
	// impact
	
	// List of literals which are recognized are references by the defuse analysis but of no meaning for us 
	
	core::ExpressionList unwanted = {
		mgr.getLangBasic().getScalarToArray(),
		mgr.getLangBasic().getRefVar(),
		mgr.getLangBasic().getRefToAnyRef()
	};

	RefList filteredRefs;
	// filtering undesired refs 
	for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) {
			core::ExpressionAddress expr = cur->getBaseExpression();
			if (core::CallExprAddress callExpr = core::dynamic_address_cast<const core::CallExpr>(expr)) {
				for(core::ExpressionList::const_iterator it = unwanted.begin(), end=unwanted.end(); it!=end; ++it) {
					if (core::analysis::isCallOf(callExpr.getAddressedNode(), *it) )
						return;
				}
			}
			// keep the ref
			filteredRefs.push_back(cur);
		});
	return filteredRefs;
}

} // end anonymous namespace 

Formula evalSize(const core::ExpressionPtr& expr) {
	// TODO: 
	return Formula();
}

struct comparator {

	bool operator()(const RefPtr& lhs, const RefPtr& rhs) const {
		return lhs->getBaseExpression() < rhs->getBaseExpression();
	}
};

Piecewise getDisplacement(const core::ExpressionPtr& expr) {
	core::NodeManager& mgr = expr->getNodeManager();
	RefList&& refs = filterUnwanted(mgr, collectDefUse(expr));

	typedef std::set<RefPtr,comparator> RefSet;
	RefSet ref_set;

	for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) { ref_set.insert(cur); } );
	assert(!ref_set.empty());

	RefPtr ref = *ref_set.begin();
	assert(ref && "No ref in expression");

	// if ( std::distance(refs.arrays_begin(), refs.arrays_end()) >= 1) {
	//RefPtr ref = *refs.arrays_begin();
	switch(ref->getType()) {
	// We do have a scalar variable which means that the displacement is zero 
	case Ref::SCALAR: return Piecewise();
	// We do have an array, therefore we need to retrieve the value of the displacement by looking at the index
	// expression utilized 
	case Ref::ARRAY: 
	{
		ArrayRefPtr arrRef = std::dynamic_pointer_cast<ArrayRef>( ref );
		assert(arrRef && "Wrong pointer cast");
		const ArrayRef::ExpressionList& indexes = arrRef->getIndexExpressions();
		if (indexes.size() == 0) {
			// no displacement has been provided 
			return Piecewise( Formula() );
		}
		if (indexes.size() == 1) {
			return core::arithmetic::toPiecewise(indexes.front());
		}
		// if we have multiple indexes then we also need to know the size of the array in order to compute the
		// size of the displacement
		assert(false && "Arrays with multiple displacements are not yet supported");
	}
	default:
		assert(false);
	}
	//}
	//std::ostringstream ss;
	//ss << "Impossible to determine displacement for argument '" << *expr << "'";
	//throw DisplacementAnalysisError(ss.str());
}


core::ExpressionPtr setDisplacement(const core::ExpressionPtr& expr, const Piecewise& displ) {
	core::NodeManager& mgr = expr->getNodeManager();
	RefList&& refs = filterUnwanted(mgr, collectDefUse(expr));

	typedef std::set<RefPtr,comparator> RefSet;
	RefSet ref_set;

	for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) { ref_set.insert(cur); } );
	assert(!ref_set.empty());

	RefPtr ref = *ref_set.begin();
	assert(ref && "No ref in expression");

	switch(ref->getType()) {
		// We do have a scalar variable which means that the displacement is zero 
		case Ref::SCALAR: assert(false && "Cannot set displacement to a scalar variable");
		// We do have an array, therefore we need to retrieve the value of the displacement by looking at the index
		// expression utilized 
		case Ref::ARRAY: 
		{
			ArrayRefPtr arrRef = std::dynamic_pointer_cast<ArrayRef>( ref );
			assert(arrRef && "Wrong pointer cast");
			const ArrayRef::ExpressionList& indexes = arrRef->getIndexExpressions();
			if (indexes.size() <= 1) {
				// the old displacement was 0, we have to create a new expression with the new displacement 
				core::IRBuilder builder(mgr);
				// Get the array expression
				core::ExpressionPtr arrRefExpr = arrRef->getBaseExpression();
				// Get the type of the array
				core::TypePtr arrType = arrRefExpr->getType();
				assert(arrType->getNodeType() == core::NT_RefType && "Expecting a ref type");
				
				// Get the type of the contained object 
				core::TypePtr nonRefTy = arrType;
				while(nonRefTy->getNodeType() == core::NT_RefType) {
					nonRefTy = nonRefTy.as<core::RefTypePtr>()->getElementType();
				}

				assert((nonRefTy->getNodeType() == core::NT_VectorType || nonRefTy->getNodeType() == core::NT_ArrayType) && 
						"expecting array or vector type");
				
				core::TypePtr elemTy = nonRefTy.as<core::SingleElementTypePtr>()->getElementType();

				return builder.callExpr(builder.refType(elemTy), 
						( nonRefTy->getNodeType() == core::NT_VectorType ? 
							builder.getLangBasic().getVectorRefElem():
							builder.getLangBasic().getArrayRefElem1D() ), 
						arrRef->getBaseExpression(), 
						builder.castExpr(builder.getLangBasic().getUInt8(), toIR(mgr, displ))
					);
			}
			// if we have multiple indexes then we also need to know the size of the array in order to compute the
			// size of the displacement
			assert(false && "Arrays with multiple displacements are not yet supported");
		}
		default:
			assert(false);
	}
}

core::ExpressionPtr getReference(const core::ExpressionPtr& expr) {

	core::NodeManager& mgr = expr->getNodeManager();
	RefList&& refs = filterUnwanted(mgr, collectDefUse(expr));

	// LOG(INFO) << refs;
	if (refs.size() == 1) {
		// We are in the easy situation, we only have a single ref therefore is the one we are looking for
		return refs.front()->getBaseExpression().getAddressedNode();
	}
	
	// multiple refs are involved in this argument, we need to find the principal one 
	// 
	// This means the address of the reference we are looking for has the shortes address
	typedef std::set<core::ExpressionAddress> AddrSet;
	AddrSet ref_addresses;

	for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) { ref_addresses.insert(cur->getBaseExpression()); } );
	assert(!ref_addresses.empty());

	return ref_addresses.begin()->getAddressedNode();
}

//~~~ FunctionSemaAnnotation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

const string FunctionSemaAnnotation::NAME = "SemaAnnotation";
const utils::StringKey<FunctionSemaAnnotation> FunctionSemaAnnotation::KEY("SemaAnnotationKey");

bool FunctionSemaAnnotation::isPure() const {
	// We go through each argument of this function 
	bool isPure = true;
	for_each(args, [&](const ReferenceInfo& cur) { 
			for_each(cur, [&] (const ReferenceInfo::AccessInfo& cur) {
					//if (cur.usage() == Ref::DEF || cur.usage() == Ref::UNKNOWN) 
					//	isPure = false;
				});
		});

	return isPure;
}

namespace {

FunctionSemaAnnotation::Args makeArgumentInfo(const std::vector<ReferenceInfo::AccessInfo>& refInfos) {
	FunctionSemaAnnotation::Args  args;
	for( const auto& ref : refInfos ) { args.push_back( ReferenceInfo( { ref } ) ); }
	return args;
}

struct PiecewiseFunctor {

	PiecewiseFunctor(const Piecewise& pw) : pw(pw) { }
	PiecewiseFunctor(const int& pw) : pw( core::arithmetic::Formula(pw) ) { }

	Piecewise operator()(const core::CallExprPtr& call) const { return pw; }

private:
	Piecewise pw;
};



} // end anonymous namespace 

using namespace insieme::utils;
using namespace insieme::core;
using namespace insieme::utils::pipeline;

// typedef Piecewise (*ToPWPtr)(const core::ExpressionPtr&);
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 // Utility MACROS 
 //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
/*#define ARG(NUM) 				std::bind(&FunctionArgument::getArgumentFor, \
									FunctionArgument(funcLit, NUM), std::placeholders::_1 \
								)*/
#define ARG(NUM)				FunctionArgument(funcLit,NUM)
//#define DISP(ARG)				std::bind(&getDisplacement,(ARG))

#define DISP(ARG)				DisplacementFunctor(ARG)
								
// Utility macro for lazy convertion of an IR expression to a formula
// #define TO_PW(ARG) 				std::bind(static_cast<ToPWPtr>(&core::arithmetic::toPiecewise), ARG)
#define TO_PW(ARG)				((ARG) >> &arithmetic::toPiecewise)

#define PW(ARG)					PiecewiseFunctor(ARG)


// Utility macro which create a lazy evaluated expressions

#define PLUS(A,B)				dup<CallExprPtr>() >> ((A)+(B))
#define MUL(A,B)				dup<CallExprPtr>() >> pipeline::makeReduction(std::multiplies<Piecewise>(),(A),(B))
#define SUB(A,B)				dup<CallExprPtr>() >> :pipeline::makeReduction(std::minus<Piecewise>(),(A),(B))
 
#define CI(USE)					insieme::analysis::LazyRange::fromUsage(USE)
#define DEF						CI(Ref::DEF)
#define USE						CI(Ref::USE)
// Utility functions 

#define SINGLE(DEF)				RANGE((DEF),PW(0),PW(1))

#define RANK_IS(VALUE)			core::arithmetic::Constraint(SUB(Formula(rank),VALUE))

// Macro which takes the value of the range 
#define RANGE_OFFSET(DEF,ADDR,OFFSET)	RANGE((DEF),DISP(ADDR),PLUS(DISP(ADDR),OFFSET))

#define RANGE(D,B,E)			LazyRange((D),(B),(E))
#define RANGE2(D,B,E,S)			LazyRange((D),(B),(E),(S))

#define NO_REF					ReferenceInfo::AccessInfo()
#define ACCESS(RANGE) 			ReferenceInfo::AccessInfo(RANGE)

#define SIZEOF(ARG)				std::bind(&evalSize, ARG)

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Utility MACROS 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//#define ARG(NUM) 				arg(funcLit, (NUM))
//#define DISP(ARG)				((ARG) >> displ())

//// Utility macro for lazy convertion of an IR expression to a formula
//#define TO_PW(ARG)		 		((ARG) >> toPiecewise())
//#define PW(ARG)					(fromFormula(ARG)) 

//#define RANGE(B,E)				(dup() >> ((B)|(E)) >> range())
//#define PLUS(A,B)				(dup() >> ((A)+(B)))
//#define SINGLE					RANGE(PW(0),PW(1))

//#define NO_REF					ReferenceInfo::AccessInfo()
//#define ACCESS(USAGE,RANGE) 	ReferenceInfo::AccessInfo(Ref::USAGE,(RANGE))

namespace {

	/**
	 * A flag used to mark node manager instances where semantic information has been loaded before.
	 */
	struct SemanticLoaded {};

}

const boost::optional<FunctionSemaAnnotation> FunctionSemaAnnotation::getFunctionSema(const core::LiteralPtr& funcLit) {
	if (std::shared_ptr<FunctionSemaAnnotation> ann = funcLit->getAnnotation( FunctionSemaAnnotation::KEY )) {
		return boost::optional<FunctionSemaAnnotation>( *ann );
	}
	// make sure semantic information is loaded and try again
	loadFunctionSemantics(funcLit->getNodeManager());
	if (std::shared_ptr<FunctionSemaAnnotation> ann = funcLit->getAnnotation( FunctionSemaAnnotation::KEY )) {
		return boost::optional<FunctionSemaAnnotation>( *ann );
	}
	return boost::optional<FunctionSemaAnnotation> ();
}


void loadFunctionSemantics(core::NodeManager& mgr) {

	LOG(DEBUG) << "Loading semantic info" << std::endl;

	// check whether it has been loaded before
	core::NodePtr flagNode = core::StringValue::get(mgr, "SemanticLoaded");
	if (flagNode->hasAttachedValue<SemanticLoaded>()) {
		return; 	// nothing to do any more
	}

	insieme::utils::Timer t("Loading function.sema");
	
	core::IRBuilder builder(mgr);
	
	// declare a variable used to refer to the processor rank
	core::VariablePtr rank = builder.variable( builder.getLangBasic().getInt4() );

	#define FUNC(Name, Type, SideEffects, args_info...)  \
	{\
	core::LiteralPtr&& funcLit = builder.literal(core::parse::parseType(mgr, Type), #Name); \
	/*LOG(INFO) << funcLit << " || " << funcLit->getType(); */\
	assert(funcLit->getType()->getNodeType() == core::NT_FunctionType && "Type in function db not a function type: " #Name); \
	FunctionSemaAnnotation::Args&& args = makeArgumentInfo({ args_info }); \
	assert(args.size() == core::static_pointer_cast<const core::FunctionType>(funcLit->getType())->getParameterTypeList().size()); \
	funcLit->addAnnotation( std::make_shared<FunctionSemaAnnotation>(args, SideEffects) ); \
	}
	#include "function_db.def"
	#undef FUNC

	t.stop();
	LOG(DEBUG) << t;

	// mark as being resolved
	flagNode->attachValue<SemanticLoaded>();
}

bool isPure(const core::LiteralPtr& funcLit) {
	if (boost::optional<FunctionSemaAnnotation> sema = FunctionSemaAnnotation::getFunctionSema(funcLit)) {
		return (*sema).isPure();
	}
	// We don't have any semantics information for this function, therefore we try decide whether the function 
	// is pure by looking at the input arguments, if they all are non-refs then we are sure the function is pure.
	vector<core::TypePtr> args = 
		core::static_pointer_cast<const core::FunctionType>(funcLit->getType())->getParameterTypeList();
	bool isPure=true;
	std::for_each(args.begin(), args.end(), [&](const core::TypePtr& cur) { 
		if(cur->getNodeType() == core::NT_RefType) { isPure = false; }
	} );
	return isPure;
}

namespace {

// Checks whether the Range provided to this access infor is an empty range or a concrete one 
struct CheckEmptyRange : public boost::static_visitor<CheckEmptyRange> {

	typedef bool result_type;

    bool operator()(const NoRange&) const { return true; }
	bool operator()(const LazyRange&) const { return false; }
};

} // end anonymous namespace 

FunctionSema extractSemantics(const core::CallExprPtr& callExpr) {

	// LOG(DEBUG) << *callExpr;

	core::LiteralPtr funcLit = core::static_pointer_cast<const core::Literal>(callExpr->getFunctionExpr());
	boost::optional<FunctionSemaAnnotation> sema = FunctionSemaAnnotation::getFunctionSema(funcLit);
	
	if(!sema) {
		// Try to do your best finding the semantics of this function 
		LOG(DEBUG) << "Tried to extract semantics for unknown function: '" 
				     << *funcLit << "' with type '" << *funcLit->getType() << "'";

		return FunctionSema(isPure(funcLit), true, FunctionSema::Accesses());
	}
	
	FunctionSema::Accesses usages;

	size_t arg=0;
	for_each((*sema).begin(), (*sema).end(), [&](const ReferenceInfo& cur) { 
		for_each(cur.begin(), cur.end(), [&](const ReferenceInfo::AccessInfo& cur) {
			
			Range r = cur.range();

			bool empty = boost::apply_visitor( CheckEmptyRange(),r );
			if (empty) { return; }

			const LazyRange& range = boost::get<const LazyRange&>(r);
			core::ExpressionAddress addr = core::Address<const core::Expression>::find(getReference(callExpr->getArgument(arg)), callExpr);
			assert(addr);
			usages.push_back( 
				FunctionSema::ReferenceAccess( 
					std::make_pair(
						FunctionSema::Reference(addr, Ref::ARRAY), // FIXME always an array?
						FunctionSema::Range(range.getUsage(callExpr), 
											range.getBegin(callExpr), 
											range.getEnd(callExpr), 
											range.getStep(callExpr)
										)
					)
				) );
		});
		++arg;
	});

	return FunctionSema((*sema).isPure(), (*sema).hasSideEffects(), usages);
}

} // end analysis namespace 
} // end insieme namespace 
