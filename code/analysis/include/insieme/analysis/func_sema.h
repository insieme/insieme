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

#include <tuple>
#include <functional>

#include "insieme/core/forward_decls.h"
#include "insieme/analysis/defuse_collect.h"
#include "insieme/core/ir_node.h"

#include "insieme/core/arithmetic/arithmetic.h"

#include "insieme/utils/printable.h"

#include <boost/variant.hpp>

namespace insieme {
namespace analysis {
	
using core::arithmetic::Formula;
using core::arithmetic::Piecewise;

/**********************************************************************************************************************
 * This class contains information relative to a useage of a particular reference. The range information tells us which
 * part of the reference is accessed, the information are kept in a symbolic way using the placeholders previously
 * introduced. This is because the range of data being accessed usually depends on the value of input arguments 
 **********************************************************************************************************************/
struct LazyRange {
	
	// A generic expression which takes the current call expression and returns the value of the bound as a formula 
	typedef std::function<Piecewise (const core::CallExprPtr&)> LazyValue;

	// Gets a plain formula and returns it as a functor 
	static LazyValue fromPiecewise(const Piecewise& f) { 
		return [f](const core::CallExprPtr& ) -> Piecewise { return f; };
	}

	static LazyValue fromFormula(const Formula& f) { 
		return [f](const core::CallExprPtr& ) -> Piecewise { return Piecewise(f); };
	}

	LazyRange(const Formula& begin, const Formula& end, const Formula& step=1) :
		begin(fromFormula(begin)), end(fromFormula(end)), step(fromFormula(step)) {	}

	LazyRange(const Piecewise& begin, const Piecewise& end, const Formula& step=1) :
		begin(fromPiecewise(begin)), end(fromPiecewise(end)), step(fromFormula(step)) {	}

	LazyRange(const LazyValue& begin, const LazyValue& end, const LazyValue& step=fromFormula(1)) :
		begin(begin), end(end), step(step) { }

	inline Piecewise getBegin(const core::CallExprPtr& expr) const { return begin(expr); }
	inline Piecewise getEnd(const core::CallExprPtr& expr) const { return end(expr); }
	inline Formula getStep(const core::CallExprPtr& expr) const { return step(expr).toFormula(); }

private:
	LazyValue begin, end, step;
};

struct LazyInvRange {
	
	typedef std::function<core::ExpressionPtr (const core::CallExprPtr&)> LazyPos;

	LazyInvRange(const LazyPos& begin, const LazyPos& end, const LazyPos& step) :
		begin(begin), end(end), step(step) { }

	inline core::ExpressionPtr getBegin(const core::CallExprPtr& expr) const { return begin(expr); }
	inline core::ExpressionPtr getEnd(const core::CallExprPtr& expr) const { return end(expr); }
	inline core::ExpressionPtr getStep(const core::CallExprPtr& expr) const { return step(expr); }

private:
	LazyPos begin, end, step;
};


// It represents an empty range, which means no range information are provided 
class NoRange { };

typedef boost::variant<LazyRange, NoRange> Range;
typedef boost::variant<LazyInvRange, NoRange> InvRange;


/**********************************************************************************************************************
 * Class which provide information of arguments of a function for which the semantics is specified. This class stores
 * whether the given argument will be only read or written within the body of the function and in both cases the class
 * should be able to say which elements will be affected by the function. The order of the provided accesses is
 * important as it is interpreted as the order on which the reference is actually accessed inside the body. For example,
 * This allows to reduce the liveness of a variable in the case we are sure its value is going to be defined again
 * before being accessed. 
 **********************************************************************************************************************/
struct ReferenceInfo {

	// The access info attach to a particular range information about the type of usage which could be DEF/USE/UNKNOWN
	
	struct AccessInfo : private std::tuple<Ref::UseType, Range, InvRange> {
		AccessInfo(
				const Ref::UseType& usage = Ref::USE, 
				const Range& range = NoRange(),
				const InvRange& inv_range = NoRange()
		) : std::tuple<Ref::UseType, Range, InvRange>(usage, range, inv_range) { }

		const Ref::UseType& usage() const { return std::get<0>(*this); }
		const Range& range() const { return std::get<1>(*this); }
		const InvRange& inv_range() const { return std::get<2>(*this); }
	};
	
	typedef std::vector<AccessInfo> Accesses;
	
	ReferenceInfo(const Accesses& usages = Accesses()): usages(usages) { }

	// Retrieve an iterator through the usages of this argument 
	inline Accesses::const_iterator begin() const { return usages.begin(); }
	inline Accesses::const_iterator end() const { return usages.end(); }

	inline size_t size() const { return usages.size(); }

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
 *********************************************************************************************************************/
struct FunctionSemaAnnotation : public core::NodeAnnotation {

	static const std::string 								NAME;
	static const utils::StringKey<FunctionSemaAnnotation> 	KEY;

	typedef std::vector<ReferenceInfo> 						Args;
	
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
						const core::NodePtr& after) const 
	{
		assert(&*ptr == this && "Annotation Pointer should point to this!");
		// always migrate the sema annotation
		after->addAnnotation( ptr );
		return true;
	}

	// Givean an IR Literal retireves Semantic informations (if attached)
	static const boost::optional<FunctionSemaAnnotation> getFunctionSema(const core::LiteralPtr& funcLit);

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
	core::ExpressionPtr operator()(const core::CallExprPtr& callExpr) const;

	std::ostream& printTo(std::ostream& out) const {
		return out << funcLit->getStringValue() << ".ARG(" << pos << ")";
	}
};

/**
 * This function analyze expressions which are passsed to a function call. In the case the function accepts a reference,
 * this method tried to identify which is the reference being accessed by the function and the displacement utilized 
 */
Piecewise getDisplacement(const core::ExpressionPtr& expr);

struct DisplacementFunctor {

	DisplacementFunctor(const FunctionArgument& argId) : argId(argId) { }

	Piecewise operator()(const core::CallExprPtr& call) const {
		return getDisplacement( argId(call) );
	}
private:
	FunctionArgument argId;
};

/**
 * This function performs the inverse operation of the getDisplacement function. Returns a new expression which replace
 * expr where the originally displacement of the main reference in expr is replaced with displ
 */
core::ExpressionPtr setDisplacement(const core::ExpressionPtr& expr, const Piecewise& new_displ);

/**
 * Utility function which is utilized to evaluate the size of a parameter. Its behaviur should be like a type trait 
 * for which the user can easily redefine the size. This is useful for example in MPI calls where types are values 
 * of an enumeration value and therefore not connected to the IR counterpart
 */
Formula evalSize(const core::ExpressionPtr& expr);

/**
 * Returns the reference being passed to a function. We can expect complex nested references to be passed to a function
 * call, this method should extract the reference which then will be read/written within the function body. 
 */
core::ExpressionPtr getReference(const core::ExpressionPtr& expr);

/**
 * Predicate which evaluates to true when the given function has is pure which means that it has no side-effects,
 * therefore calling the function multiple times with the same input configuration will always produce the same results.
 */
bool isPure(const core::LiteralPtr& funcLit);

/**
 * Main entry point. This function loads all the information from the function_db.def XMacro file and set the semantics
 * of the functions within this node manager. This is done by creating a function literal containing the name and the
 * tyoe of the function and successively attach the FunctionSemaAnnotation which stores the semantic information. This 
 * method should be invoked before or after the frontend is executed or whenever a transformation introduced new
 * literals. However it should be executed before any analysis is performed. 
 */
void loadFunctionSemantics(core::NodeManager& mgr);



/**********************************************************************************************************************
 * CALL SITE INFO
 *
 * Merge the information stored in the annotation with the call expression present at the call site. This will return
 * the semantics of a particular call expression stating the references being accessed within the body of the literal
 * and the range of values interested by the call. 
 **********************************************************************************************************************/

struct FunctionSema {
	
	class Reference {
		core::ExpressionAddress addr;
		Ref::UseType 			usage;
		Ref::RefType			type;

	public:
		Reference(const core::ExpressionAddress& 	addr, 
			const Ref::UseType& 					usage,
			const Ref::RefType& 					type) : 
		addr(addr), usage(usage), type(type) { }
		
		inline const core::ExpressionAddress& getReference() const { return addr; }
		
		inline const Ref::UseType& getUsage() const { return usage; }
		inline const Ref::RefType& getType() const { return type; }
	};

	typedef std::tuple<Piecewise, Piecewise, Formula> Range;

	typedef std::pair<Reference, Range> 	ReferenceAccess;
	typedef std::vector<ReferenceAccess> 	Accesses;

	FunctionSema(bool pure, bool sideEffects, const Accesses& accesses) : 
		 pure(pure), sideEffects(sideEffects), accesses(accesses) { }

	bool isPure() const { return pure; }
	bool hasSideEffects() const { return sideEffects; }

	Accesses::const_iterator accesses_begin() const { return accesses.begin(); }
	Accesses::const_iterator accesses_end() const { return accesses.end(); }

	bool containsReferenceAccesses() const { return !accesses.empty(); }

private:
	bool 		pure;
	bool 		sideEffects;
	Accesses 	accesses;

};



/**********************************************************************************************************************
 * This class gives information of the semantics of a function at the call site. If the called literal expression has
 * semantic annotation associated, then the actual semantics is returned, otherwise the function will try to analyze the
 * semantics of the function in a conservative way.
 **********************************************************************************************************************/

FunctionSema extractSemantics(const core::CallExprPtr& callExpr);


} // end analysis namespace
} // end insieme namespace 

