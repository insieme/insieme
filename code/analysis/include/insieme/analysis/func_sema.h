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

#pragma once 

#include "insieme/core/forward_decls.h"
#include "insieme/analysis/defuse_collect.h"
#include "insieme/core/ir_node.h"

#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/utils/printable.h"
#include <functional>

namespace insieme {
namespace analysis {
	
using core::arithmetic::Formula;

/**********************************************************************************************************************
 * This class contains information relative to a useage of a particular reference. The range information tells us which
 * part of the reference is accessed, the information are kept in a symbolic way using the placeholders previously
 * introduced. This is because the range of data being accessed usually depends on the value of input arguments 
 */
struct Range {
	
	// A generic expression which takes the current call expression and returns the value of the bound as a formula 
	typedef std::function<Formula (const core::CallExprPtr&)> LazyValue;

	// Gets a plain formula and returns it as a functor 
	static LazyValue fromFormula(const Formula& f) { 
		return [f](const core::CallExprPtr& ) -> Formula { return f; };
	} 

	Range() : norange(true) { }

	Range(const Formula& begin, const Formula& end, const Formula& step=1) :
		begin(fromFormula(begin)), end(fromFormula(end)), step(fromFormula(step)), norange(false) {	}

	Range(const LazyValue& begin, const LazyValue& end, const LazyValue& step=fromFormula(1)) :
		begin(begin), end(end), step(step), norange(false) { }

	inline Formula getBegin(const core::CallExprPtr& expr) const { return begin(expr); }
	inline Formula getEnd(const core::CallExprPtr& expr) const { return end(expr); }
	inline Formula getStep(const core::CallExprPtr& expr) const { return step(expr); }

	inline bool isNoRange() const { return norange; }

private:
	LazyValue begin, end, step;
	bool norange;
};


/**********************************************************************************************************************
 * Class which provide information of arguments of a function for which the semantics is specified. This class stores
 * whether the given argument will be only read or written within the body of the function and in both cases the class
 * should be able to say which elements will be affected by the function. The order of the provided accesses is
 * important as it is interpreted as the order on which the reference is actually accessed inside the body. For example,
 * This allows to reduce the liveness of a variable in the case we are sure its value is going to be defined again
 * before being accessed. 
 */
struct ReferenceInfo {

	// The access info attach to a particular range information about the type of usage which could be DEF/USE/UNKNOWN
	
	struct AccessInfo : private std::pair<Ref::UseType, Range> {
		AccessInfo(const Ref::UseType& usage = Ref::USE, const Range& range = Range()) : 
			std::pair<Ref::UseType, Range>(usage, range) { }

		const Ref::UseType& usage() const { return first; }
		const Range& range() const { return second; }
	};
	
	typedef std::vector<AccessInfo> Accesses;
	
	ReferenceInfo(const Accesses& usages = Accesses()): usages(usages) { }

	// Retrieve an iterator through the usages of this argument 
	inline Accesses::const_iterator begin() const { return usages.begin(); }
	inline Accesses::const_iterator end() const { return usages.end(); }

	// Return true when this argument has no usages which means that type of the relative input arguments is not a
	// reference 
	inline bool isConstant() const { return usages.empty(); }

private: 
	Accesses usages;
};

/**********************************************************************************************************************
 * For a known function this class provides information about its sematics.  I.e. whether the function has sideeffects
 * or not, additionally it says whether the input arguments will be read or written and if possible the range of values
 * which will be touched by this function 
 */
struct FunctionSemaAnnotation : public core::NodeAnnotation {

	static const std::string 								NAME;
	static const utils::StringKey<FunctionSemaAnnotation> 	KEY;

	typedef std::vector<ReferenceInfo> Args;
	
	FunctionSemaAnnotation(const Args& args, bool sideEffects=false) : args(args), sideEffects(sideEffects) { }

	// Returns true if the function is considered pure. This is deduced by analyzing the usage of this function's
	// arguments. If all the arguments are read and nothing is being written, then the function is marked as pure. If
	// given references are written, then the function will be marked as non pure. 
	bool isPure() const;

	// Returns true if the function is known to have side effects. Note that a function can be pure but still have side
	// effects. For example functions working on files. Because this information cannot be subsumed by looking at the
	// input arguments, it has to be provided by the user.
	inline bool hasSideEffects() const { return sideEffects; }

	inline std::ostream& printTo(std::ostream& out) const {
		for_each(args, [&](const ReferenceInfo& cur) {
				for_each(cur, [&](const ReferenceInfo::AccessInfo& cur) {
						out << Ref::useTypeToStr(cur.usage()) << std::endl; 
					});
			});
		return out;
	}
	
	inline const std::string& getAnnotationName() const { return NAME; }

	inline const utils::AnnotationKey* getKey() const { return &KEY; }

	inline bool migrate(const core::NodeAnnotationPtr& ptr, 
						const core::NodePtr& before, 
						const core::NodePtr& after) const { return true; }

	// Givean an IR Literal retireves Semantic informations (if attached)
	static const boost::optional<FunctionSemaAnnotation> getFunctionSema(const core::LiteralPtr& funcLit) {
		if (std::shared_ptr<FunctionSemaAnnotation> ann = funcLit->getAnnotation( FunctionSemaAnnotation::KEY )) {
			return boost::optional<FunctionSemaAnnotation>( *ann );
		}
		return boost::optional<FunctionSemaAnnotation> ();
	}

	inline Args::const_iterator begin() const { return args.begin(); }
	inline Args::const_iterator end() const { return args.end(); }

private:
	Args args;
	bool sideEffects;
};

//==== Utility Functions utilized for extracting information from the arguments of a call expression ===================

// Exception to be thrown when the analysis cannot figure out the displacemnt of the given expression 
struct DisplacementAnalysisError : public std::logic_error {
	DisplacementAnalysisError(const std::string& msg) : logic_error(msg) { }
};

/**
 * This function analyze expressions which are passsed to a function call. In the case the function accepts a reference,
 * this method tried to identify which is the reference being accessed by the function and the displacement utilized 
 */
Formula getDisplacement(const core::ExpressionPtr& expr);

/**
 * Returns the reference being passed to a function. We can expect complex nested references to be passed to a function
 * call, this method should extract the reference which then will be read/written within the function body. 
 */
core::ExpressionPtr getReference(const core::ExpressionPtr& expr);

/**
 * Main entry point. This function loads all the information from the function_db.def XMacro file and set the semantics
 * of the functions within this node manager. This is done by creating a function literal containing the name and the
 * tyoe of the function and successively attach the FunctionSemaAnnotation which stores the semantic information. This 
 * method should be invoked before or after the frontend is executed or whenever a transformation introduced new
 * literals. However it should be executed before any analysis is performed. 
 */
void loadFunctionSemantics(core::NodeManager& mgr);

class Usage {

	core::ExpressionPtr ref;
	Ref::UseType 		usage;
	Formula begin, end, stride;

public:
	Usage(const core::ExpressionPtr& ref, 
		const Ref::UseType& usage, 
		const Formula& begin=0, 
		const Formula& end=0, 
		const Formula& stride=1) :
	ref(ref), usage(usage), begin(begin), end(end), stride(stride) { }
	
	inline const Formula& getBegin() const { return begin; }
	inline const Formula& getEnd() const { return end; }
	inline const Formula& getStride() const { return stride; }
	inline const Ref::UseType& getUsage() const { return usage; }
	inline const core::ExpressionPtr getReference() const { return ref; }
};

typedef std::vector<Usage> 			UsageVect;

class FunctionSema {
	
	bool 		sideEffects;
	bool 		pure;
	UsageVect 	usages;

public:
	FunctionSema(bool sideEffects, bool pure, const UsageVect& usages) : 
		sideEffects(sideEffects), pure(pure), usages(usages) { }

	bool isPure() const { return pure; }
	bool hasSideEffects() const { return sideEffects; }

	UsageVect::const_iterator usages_begin() const { return usages.begin(); }
	UsageVect::const_iterator usages_end() const { return usages.end(); }

};

/**********************************************************************************************************************
 * This class gives information of the semantics of a function at the call site. If the called literal expression has
 * semantic annotation associated, then the actual semantics is returned, otherwise the function will try to analyze the
 * semantics of the function in a conservative way.
 */
FunctionSema extractSemantics(const core::CallExprPtr& callExpr);


} // end analysis namespace
} // end insieme namespace 

