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
#include "insieme/utils/printable.h"
#include "insieme/utils/set_utils.h"
#include <vector>
#include <set>
#include <memory>

#include "boost/operators.hpp"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_expressions.h"

#include "boost/variant.hpp"

namespace insieme {

namespace core {
typedef std::vector<StatementPtr> StatementList;
typedef insieme::utils::set::PointerSet<StatementPtr> StatementSet;
}// end core namespace 

namespace analysis {

class Ref;

typedef std::shared_ptr<Ref> RefPtr; 

/**************************************************************************************************
 * Class Ref represent a generic IR ref which can be either assigned or read. In this context 
 * a Ref can be either a scalar variable, an array or a vector (having a ref type), a struct/class
 * member or the return value of a call expression returning a ref. 
 *************************************************************************************************/
struct Ref : public utils::VirtualPrintable {

	/**********************************************************************************************
	 * possible usage of a variable can be of three types: 
	 *  USE: 	 the variable is being accessed, therefore the memory location is read and not 
	 *  		 modified 
	 *  DEF: 	 the variable is being redefined (declaration and assignment), this means that the
	 *   	  	 memory associated to that variable is being written 
	 * 	UNKNOWN: the variable is being used as input parameter to a function which can either read
	 * 	         or modify the value. UNKNOWN type of usages can be refined through advanced 
	 * 	         dataflow analysis
	 * 	ANY: 	 utilized in the RefList class to iterate through any type of usage
	 *********************************************************************************************/
	enum UseType { ANY_USE=-1, DEF, USE, UNKNOWN };

	/*********************************************************************************************
	 * Possible type of references are:
	 * SCALAR: reference to scalar variables 
	 * ARRAY:  reference to arrays 
	 * CALL:   return value of a function returning a reference 
	 * ANY:    used in the RefList class in order to iterate through any reference type 
	 ********************************************************************************************/
	enum RefType { ANY_REF=-1, SCALAR, ARRAY, MEMBER, CALL };

	std::ostream& printTo(std::ostream& out) const;

	inline const UseType& getUsage() const { return usage; }
	
	inline const RefType& getType() const { return type; }

	inline const core::ExpressionAddress& getBaseExpression() const { return baseExpr; }
	
	static std::string useTypeToStr(const UseType& usage);
	static std::string refTypeToStr(const RefType& type);

protected:
	Ref(const RefType& type, const core::ExpressionAddress& expr, const UseType& usage = USE);
	
	// Points to the base expression: 
	// 	 this can be either a scalar variable, an array or a call to a function 
	core::ExpressionAddress baseExpr;

private:
	// Define the type of this reference 
	RefType type;

	// Define the use for this expression  
	UseType usage; 
};

class ScalarRef;
typedef std::shared_ptr<ScalarRef> ScalarRefPtr;

/**************************************************************************************************
 * Thsi class represents a reference to a scalar variable. Because the baseExpr points to the
 * variable, we can safely cast the base expression to a variable reference  
 *************************************************************************************************/
struct ScalarRef : public Ref {

	ScalarRef(const core::VariableAddress& var, const Ref::UseType& usage);

	core::VariableAddress getVariable() const;
	std::ostream& printTo(std::ostream& out) const;
};

class MemberRef;
typedef std::shared_ptr<MemberRef> MemberRefPtr;

/**************************************************************************************************
 * This class represents a reference to a struct member. It provides utility methods to easily
 * access to the field name being accessed and the type of the composite 
 *************************************************************************************************/
struct MemberRef: public Ref {

	typedef boost::variant<
		core::StructTypePtr, 
		core::UnionTypePtr,
		core::RecTypePtr,
		core::GenericTypePtr
	> MemberType;

	MemberRef(const core::ExpressionAddress& memberAcc, const UseType& usage);

	template <typename Type>
	inline const core::Pointer<Type>& getAs() const { 
		return boost::get<Type>(type); 
	}
	inline const core::LiteralPtr& getIdentifier() const { 
		return identifier; 
	}
	std::ostream& printTo(std::ostream& out) const;

private:
	MemberType 			type;
	core::LiteralPtr 	identifier;
};

class ArrayRef;
typedef std::shared_ptr<ArrayRef> ArrayRefPtr;

/**************************************************************************************************
 * In the case of arrays (or vectors), we also store the list of expressions used to index each of 
 * the array dimensions. The baseExpr is used to point at the entire array subscript expression. 
 *************************************************************************************************/
struct ArrayRef : public Ref { 
	
	typedef std::vector<core::ExpressionAddress> ExpressionList;  

	ArrayRef(const core::ExpressionAddress&  arrayVar, 
			 const ExpressionList& 			 idxExpr, 
			 const core::ExpressionAddress&  exprPtr, 
			 const UseType& 				 usage = USE) 
	: Ref(Ref::ARRAY, arrayVar, usage), 
	  exprPtr(exprPtr), 
	  idxExpr(idxExpr) { }

	std::ostream& printTo(std::ostream& out) const;	

	inline const ExpressionList& getIndexExpressions() const { return idxExpr; }
	
	inline const core::ExpressionAddress& getSubscriptExpression() const { 
		return !exprPtr ? baseExpr : exprPtr; 
	}

private:
	core::ExpressionAddress exprPtr;
	ExpressionList 			idxExpr;
};

class CallRef;
typedef std::shared_ptr<CallRef> CallRefPtr;

/**************************************************************************************************
 * This class represents a reference to a struct member. It provides utility methods to easily
 * access to the field name being accessed and the type of the composite 
 *************************************************************************************************/
struct CallRef: public Ref {

	CallRef(const core::CallExprAddress& callExpr, const UseType& usage);

	std::ostream& printTo(std::ostream& out) const;

	core::CallExprAddress getCallExpr() const;
};


// Store the set of refs found by the visitor 
class RefList: public std::vector<RefPtr>, public utils::Printable {
	
public:
	template <class T>
	class ref_iterator : 
		public boost::forward_iterator_helper<ref_iterator<T>, const std::shared_ptr<T>> { 
		
		RefList::const_iterator it, end;
		Ref::RefType type;
		Ref::UseType usage; 

		void inc(bool first=false) {
			// iterator not valid, therefore increment not allowed
			if (it == end) { return; }

			if (first && (type == Ref::ANY_REF || (*it)->getType() == type) && 
					(usage == Ref::ANY_USE || (*it)->getUsage() == usage) )	
			{ return; }
			++it; // increment the iterator
			while(it != end &&	!(type == Ref::ANY_REF || (*it)->getType() == type) && 
								 (usage == Ref::ANY_USE || (*it)->getUsage() == usage)) 
			{ ++it; } // increment until we find an element which satisfy the filter
		}
	public:
		ref_iterator( RefList::const_iterator 	begin, 
					  RefList::const_iterator 	end, 
	  				  Ref::RefType 				type=Ref::ANY_REF, 
					  Ref::UseType 				usage=Ref::ANY_USE ) 
			: it(begin), end(end), type(type), usage(usage) { inc(true); }

		inline const std::shared_ptr<T> operator*() const { 
			assert(it != end && "Iterator out of bounds"); 
			return std::static_pointer_cast<T>(*it);
		}

		inline ref_iterator<T>& operator++() { inc(); return *this; }

		inline bool operator==(const ref_iterator<T>& rhs) const { 
			return it == rhs.it && usage == rhs.usage && type == rhs.type;
		}
	};

	// Iterates through all the references 
	inline RefList::ref_iterator<Ref> refs_begin(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<Ref>(begin(), end(), Ref::ANY_REF, usage);
	}
	inline RefList::ref_iterator<Ref> refs_end(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<Ref>(end(), end(), Ref::ANY_REF, usage); 
	}

	// Iterates through the scalar references only 
	inline RefList::ref_iterator<ScalarRef> scalars_begin(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<ScalarRef>(begin(), end(), Ref::SCALAR, usage); 
	}
	inline RefList::ref_iterator<ScalarRef> scalars_end(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<ScalarRef>(end(), end(), Ref::SCALAR, usage); 
	}

	// Iterates through the array references only 
	inline RefList::ref_iterator<ArrayRef> arrays_begin(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<ArrayRef>(begin(), end(), Ref::ARRAY, usage); 
	}
	inline RefList::ref_iterator<ArrayRef> arrays_end(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<ArrayRef>(end(), end(), Ref::ARRAY, usage); 
	}

	// Iterates through the Member references only 
	inline RefList::ref_iterator<MemberRef> members_begin(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<MemberRef>(begin(), end(), Ref::MEMBER, usage); 
	}
	inline RefList::ref_iterator<MemberRef> members_end(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<MemberRef>(end(), end(), Ref::MEMBER, usage); 
	}

	// Iterates through the call expr references only 
	inline RefList::ref_iterator<CallRef> calls_begin(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<CallRef>(begin(), end(), Ref::CALL, usage); 
	}
	inline RefList::ref_iterator<CallRef> calls_end(const Ref::UseType& usage=Ref::ANY_USE) { 
		return ref_iterator<CallRef>(end(), end(), Ref::CALL, usage); 
	}

	// Allows this list to be printed to some stream
	std::ostream& printTo(std::ostream& out) const;
};

/**************************************************************************************************
 * Main entry method, visits the IR starting from the root node collecting refs. The list of
 * detected refs is returned to the caller.
 *
 * The method also takes a set of statements which should not be skipped during the analysis. This
 * is useful in the case the def-use analysis has been performed on a sub tree of the current root
 * and we want to perform the analysis only on the remaining part of the tree 
 *************************************************************************************************/
RefList collectDefUse(const core::NodePtr& root, const core::StatementSet& skipList = core::StatementSet());

RefList collectDefUse(const core::NodeAddress& root, const core::StatementSet& skipList = core::StatementSet());

} // end namespace analysis 
} // end namesapce insieme 
