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

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace analysis {

/**********************************************************************************************************************
 * A placeholder for referring to a particular argument of a function literal. 
 * 
 * The purpose of this class is the ability to being able to refer to a particular elemenet of a call expression in a
 * generic way. 
 */
class FunctionArgument : public utils::Printable {
	// The function literal to which we refer to 
	core::LiteralPtr funcLit;

	// The argument position within the function literal
	size_t pos;
public:
	FunctionArgument(const core::LiteralPtr& funcLit, size_t pos) : funcLit(funcLit), pos(pos) { }

	// Given a concrete call expression, this method returns the argument to which this object is referring
	core::ExpressionPtr getArgumentFor(const core::CallExprPtr& callExpr) const;

	std::ostream& printTo(std::ostream& out) const {
		return out << funcLit->getStringValue() << ".ARG(" << pos << ")";
	}
};

// Given a concrete call expression, this method returns the argument to which this object is referring
core::ExpressionPtr FunctionArgument::getArgumentFor(const core::CallExprPtr& callExpr) const {
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
		mgr.getLangBasic().getRefVar()
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

core::arithmetic::Formula getDisplacement(const core::ExpressionPtr& expr) {
	core::NodeManager& mgr = expr->getNodeManager();
	RefList&& refs = filterUnwanted(mgr, collectDefUse(expr));

	// Filtered references
	// std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ LOG(DEBUG) << *cur;	});

	if (refs.size() == 1) {
		// this is the lucky case, we do have only 1 reference in this argument so we are sure we are referring to it
		RefPtr ref = refs.front();
		switch(ref->getType()) {
			// We do have a scalar variable which means that the displacement is zero 
			case Ref::SCALAR: return 0;
			// We do have an array, therefore we need to retrieve the value of the displacement by looking at the index
			// expression utilized 
			case Ref::ARRAY: 
			{
				ArrayRefPtr arrRef = std::dynamic_pointer_cast<ArrayRef>( ref );
				assert(arrRef && "Wrong pointer cast");
				const ArrayRef::ExpressionList& indexes = arrRef->getIndexExpressions();
				if (indexes.size() == 0) {
					// no displacement has been provided 
					return Formula(0);
				}
				if (indexes.size() == 1) {
					return core::arithmetic::toFormula(indexes.front());
				}
				// if we have multiple indexes then we also need to know the size of the array in order to compute the
				// size of the displacement
				assert(false && "Arrays with multiple displacements are not supported");
			}
			default:
				assert(false);
		}
	}
	std::ostringstream ss;
	ss << "Impossible to determine displacement for argument '" << *expr << "'";
	throw DisplacementAnalysisError(ss.str());
}

core::ExpressionPtr getReference(const core::ExpressionPtr& expr) {
	core::NodeManager& mgr = expr->getNodeManager();
	RefList&& refs = filterUnwanted(mgr, collectDefUse(expr));

	if (refs.size() == 1) {
		// We are in the easy situation, we only have a single ref therefore is the one we are looking for
		return refs.front()->getBaseExpression().getAddressedNode();
	}
	
	// multiple refs are involved in this argument, we need to find the principal one 
	// 
	// This means the address of the reference we are looking for have the shortes address
	typedef std::set<core::ExpressionAddress> AddrSet;
	AddrSet ref_addresses;

	for_each(refs.begin(), refs.end(), [&](const RefPtr& cur) { ref_addresses.insert(cur->getBaseExpression()); } );
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
					if (cur.usage() == Ref::DEF || cur.usage() == Ref::UNKNOWN) 
						isPure = false;
				});
		});

	return isPure;
}

namespace {

FunctionSemaAnnotation::Args makeArgumentInfo(const ReferenceInfo::AccessInfo& cur) {
	return FunctionSemaAnnotation::Args( { ReferenceInfo( { cur } ) } );
}

template <class ...T> 
FunctionSemaAnnotation::Args makeArgumentInfo(const ReferenceInfo::AccessInfo& first, const ReferenceInfo::AccessInfo& second, T... rest) {
	FunctionSemaAnnotation::Args restArgs = makeArgumentInfo(second, rest...);
	FunctionSemaAnnotation::Args ret(restArgs.size()+1);
	std::copy(restArgs.begin(), restArgs.end(), ret.begin()+1);
	ret[0] = ReferenceInfo( { first } );
	return ret;
}

} // end anonymous namespace 

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Utility MACROS 
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define ARG(NUM) \
			std::bind(&FunctionArgument::getArgumentFor, FunctionArgument(funcLit, NUM), std::placeholders::_1)

// Build a functor which returns the displacement for the given argument which will be resolved when a call expression
// will be bound
#define DISPL(ARG)   			std::bind(&getDisplacement, ARG)

typedef Formula (*ToFormulaPtr)(const core::ExpressionPtr&);
// Utility macro for lazy convertion of an IR expression to a formula
#define TO_FORMULA(ARG) 		std::bind( static_cast<ToFormulaPtr>(&core::arithmetic::toFormula), ARG)

#define FORMULA(ARG)			(Range::fromFormula(ARG))

// Utility macro which create a lazy evaluated expressions
#define PLUS(A,B)				std::bind( std::plus<core::arithmetic::Formula>(), 			(A), (B) )
#define MUL(A,B)				std::bind( std::multiplies<core::arithmetic::Formula>(), 	(A), (B) )
#define SUB(A,B)				std::bind( std::minus<core::arithmetic::Formula>(), 		(A), (B) )
#define MOD(A,B)				std::bind( std::modulus<core::arithmetic::Formula>(), 		(A), (B) )
#define DIV(A,B)				std::bind( std::divides<core::arithmetic::Formula>(), 		(A), (B) )

#define NO_REF					ReferenceInfo::AccessInfo()
#define ACCESS(USAGE,RANGE) 	ReferenceInfo::AccessInfo(Ref::USAGE, RANGE)

void loadFunctionSemantics(core::NodeManager& mgr) {
	core::IRBuilder builder(mgr);
	#define FUNC(Name, Type, SideEffects, args_info...)  \
	{\
	core::LiteralPtr&& funcLit = builder.literal(core::parse::parseType(mgr, Type), #Name); \
	FunctionSemaAnnotation::Args&& args = makeArgumentInfo(args_info); \
	assert(args.size() == core::static_pointer_cast<const core::FunctionType>(funcLit->getType())->getParameterTypeList().size()); \
	funcLit->addAnnotation( std::make_shared<FunctionSemaAnnotation>( args, SideEffects ) ); \
	}
	#include "function_db.def"
	#undef FUNC
}

namespace {

bool isPure(const core::CallExprPtr& callExpr) {
	core::LiteralPtr funcLit = core::static_pointer_cast<const core::Literal>(callExpr->getFunctionExpr());
	if (boost::optional<FunctionSemaAnnotation> sema = FunctionSemaAnnotation::getFunctionSema(funcLit)) {
		return (*sema).isPure();
	}
	// We don't have any semantics information for this function, therefore we try decide whether the function 
	// is pure by looking at the input arguments, if they all are non-refs then we are sure the function is pure.
	const vector<core::ExpressionPtr>& args = callExpr->getArguments();
	bool isPure=true;
	std::for_each(args.begin(), args.end(), [&](const core::ExpressionPtr& cur) { 
		if(cur->getType()->getNodeType() == core::NT_RefType) { isPure = false; }
	} );
	return isPure;
}

} // end anonymous namespace 

FunctionSema extractSemantics(const core::CallExprPtr& callExpr) {

	core::LiteralPtr funcLit = core::static_pointer_cast<const core::Literal>(callExpr->getFunctionExpr());
	boost::optional<FunctionSemaAnnotation> sema = FunctionSemaAnnotation::getFunctionSema(funcLit);
	
	if(!sema) {
		// Try to do your best finding the semantics of this function 
		LOG(WARNING) << "Tried to extract semantics for unknown function: '" << *funcLit << "' with type '" << *funcLit->getType() << "'";
		return FunctionSema(false, true, UsageVect());
	}
	
	UsageVect usages;

	size_t arg=0;
	for_each((*sema).begin(), (*sema).end(), [&](const ReferenceInfo& cur) { 
		for_each(cur.begin(), cur.end(), [&](const ReferenceInfo::AccessInfo& cur) {
			
			if (cur.range().isNoRange()) { return; }
			
			const Range& range = cur.range();
			usages.push_back( Usage(getReference(callExpr->getArgument(arg)), 
								  cur.usage(), 
								  range.getBegin(callExpr),  
								  range.getEnd(callExpr), 
								  range.getStep(callExpr))
							);
		});
		++arg;
	});

	return FunctionSema((*sema).isPure(), (*sema).hasSideEffects(), usages);
}

} // end analysis namespace 
} // end insieme namespace 
