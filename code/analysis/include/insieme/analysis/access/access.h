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
#include "insieme/core/ir_address.h"

#include "insieme/analysis/access/unified_address.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/constraint.h"

#include "insieme/analysis/tmp_var_map.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

namespace insieme { 
namespace analysis { 
namespace access {

	typedef utils::CombinerPtr<polyhedral::AffineFunction> ConstraintPtr;
	
	#define DEFINE(CLS) \
		class CLS; \
		typedef std::shared_ptr<const CLS> CLS##Ptr;

	//=== forward declarations ========================================================================
	DEFINE(Access);
	DEFINE(BaseAccess);
	DEFINE(AccessDecorator);
	DEFINE(Ref);
	DEFINE(Deref);
	DEFINE(Member);
	DEFINE(Subscript);

	enum class AccessType { AT_BASE, AT_REF, AT_DEREF, AT_MEMBER, AT_SUBSCRIPT };

	/** 
	 * Abstract the access to either an L-Value or to an R-Value. 
	 *
	 * This representation can be used to address expressions in the IR based on their address or in
	 * a CFG based on the cfg::Address. For this purpose the UnifiedAddress abstraction is utilized.
	 */
	class Access : public utils::Printable { 

		/* Address of the expression */
		UnifiedAddress 	addr;
		AccessType 	   	type;
		bool 			final;

	public:
		Access(const UnifiedAddress& addr, const AccessType& type, bool final=false) : 
			addr(addr), type(type), final(final)  { }

		inline const UnifiedAddress& getAddress() const { 
			return addr; 
		}

		inline const AccessType& getType() const { 
			return type; 
		}

		bool isBaseAccess() const { return type == AccessType::AT_BASE; }

		/**
		 * Returns true when the expression is an R-Value
		 */
		bool isReference() const;

		inline bool isFinal() const { return final; }

		/** 
		 * For expressions depending on a context (for example array accesses based on an index
		 * expression) this function says whether this representation is dependent on context
		 * information
		 */
		virtual bool isContextDependent() const { return false; }

		/** 
		 * Compare two access for equality
		 */
		inline bool operator==(const Access& other) const { 
			// Test the trivial case first 
			if (this == &other) { return true; }
			return addr == other.addr;
		}

		virtual std::ostream& printTo(std::ostream& out) const = 0;

		virtual ~Access() { }
	};



	/** 
	 * An access to an IR variable. Since the IR allows variables as L-Values and R-Values is not
	 * given that a base access is an L-Value. 
	 */
	class BaseAccess : public Access {

	public:
		BaseAccess(const UnifiedAddress& addr, bool final=false) : 
			Access(addr,AccessType::AT_BASE, final) { }

		core::VariablePtr getVariable() const { 
			return getAddress().getAddressedNode().as<core::VariablePtr>();
		}

		inline const BaseAccess& getRoot() const { return *this; }
		inline BaseAccessPtr getRootPtr() const { assert(false); }

		virtual std::ostream& printTo(std::ostream& out) const;
	};


	struct AccessDecorator : public Access {

		AccessDecorator(const UnifiedAddress& 	addr, 
						const AccessPtr& 		subAccess, 
						const AccessType& 		type,
						bool 					final = false
		) :
			Access(addr, type, final), subAccess(subAccess) { }

		inline const AccessPtr& getSubAccess() const { return subAccess; }

		virtual AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const = 0;

	private:

		AccessPtr 	subAccess;
		AccessType 	type;
	};

	
	class Ref: public AccessDecorator {

	public:
		Ref(const UnifiedAddress& addr,
			const AccessPtr&	  subAccess) : AccessDecorator(addr, subAccess, AccessType::AT_REF) { }

		inline AccessDecoratorPtr switchSubAccess(const AccessPtr& newRoot) const {
			return std::make_shared<Ref>(getAddress(), newRoot);
		}

		std::ostream& printTo(std::ostream& out) const;
	};


	class Deref : public AccessDecorator {

	public:
		Deref(const UnifiedAddress& 	addr, 
			  const AccessPtr& 			subAccess,
			  bool						final = false
		) :
			AccessDecorator(addr, subAccess, AccessType::AT_DEREF, final) { }

		inline AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const {
			return std::make_shared<Deref>(getAddress(), sub);
		}

		virtual std::ostream& printTo(std::ostream& out) const;
	};


	class Member: public AccessDecorator {

		core::LiteralPtr	member;

	public:
		Member(const UnifiedAddress& 	addr, 
			   const AccessPtr& 		subAccess, 
			   const core::LiteralPtr& 	member,
			   bool 					final = false
		) :
			AccessDecorator(addr, subAccess, AccessType::AT_MEMBER, final), 
			member(member) { }

		inline const core::LiteralPtr& getMember() const { return member; }

		inline AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const {
			return std::make_shared<Member>(getAddress(), sub, member);
		}

		virtual std::ostream& printTo(std::ostream& out) const;

	};


	class Subscript : public AccessDecorator {

		core::NodeAddress			ctx;
		polyhedral::IterationVector iv;
		ConstraintPtr 				range;

	public:
		Subscript(const UnifiedAddress& 			addr, 
				const AccessPtr& 					subAccess, 
				bool								final   = false,
				const core::NodeAddress& 			ctx 	= core::NodeAddress(),
				const polyhedral::IterationVector& 	iterVec = polyhedral::IterationVector(), 
				const ConstraintPtr 				range 	= ConstraintPtr()
		) : 
			AccessDecorator(addr, subAccess, AccessType::AT_SUBSCRIPT, final), 
			ctx(ctx),
			iv(iterVec), 
			range( polyhedral::cloneConstraint(iv, range) ) { }

		
		inline const core::NodeAddress& getContext() const { return ctx; }

		inline const ConstraintPtr& getRange() const { return range; }

		inline void setRange(const ConstraintPtr& range) { this->range = range; }

		inline bool isContextDependent() const {
			
			// we have an array access, now check whether we have a bound expression limiting the range of
			// accessed elements 
			if (range) {
				// if there are parameters in this access, then this access depends on the context 
				if (iv.getParameterNum() > 0) 
					return true; 
			}

			return false;
		}

		inline const polyhedral::IterationVector& getIterationVector() const {
			return iv;
		}
			
		inline AccessDecoratorPtr switchSubAccess(const AccessPtr& sub) const {
			return std::make_shared<Subscript>(getAddress(), sub, false, ctx, iv, range);
		}

		virtual std::ostream& printTo(std::ostream& out) const;
	};


	/** 
	 * Returns the root access given a composed access 
	 */
	inline BaseAccessPtr getRoot(const AccessPtr& access) {
		if (auto base = std::dynamic_pointer_cast<const BaseAccess>(access)) { return base; }
		
		auto dec = std::static_pointer_cast<const AccessDecorator>(access);
		return getRoot(dec->getSubAccess());
	}

	/**
	 * Replace the root node of access with the access represented by newRoot. 
	 *
	 * This routine is useful in those situations when two accesses need to be concatenated together
	 * to form a new access 
	 */
	AccessPtr switchRoot(const AccessPtr& access, const AccessPtr& newRoot);

	
	/** 
	 * Utility which casts one access of a generic type to a concrete type.
	 */
	template <class T>
	std::shared_ptr<const T> cast(const AccessPtr& access) {
	//	astert(typeid(access) == typeid(std::shared_ptr<const T>()) && 
	//			"Invalid cast");
		return std::static_pointer_cast<const T>(access);
	}


	
	typedef std::vector<AccessPtr> AccessVector;

	// ======================= Utility Functions ================================================ //
	// Utility functions utilized to extract an address from any given IR expression starting either 
	// from an ir-address or from a CFG block.
	//
	// If the given expression is not a direct access (for example an expression in the form a+b)
	// then the NotAnAccess exception will be thrown 
	//
	struct NotAnAccessException : public std::logic_error {

		NotAnAccessException(const std::string& str) : 
			std::logic_error("Not an access: " + str) { }
	};

	AccessPtr getImmediateAccess(core::NodeManager& 	mgr, 
								 const UnifiedAddress& 	expr, 
								 const TmpVarMap& 		tmpVarMap=TmpVarMap(),
								 bool					final=true);


	inline AccessPtr getImmediateAccess(core::NodeManager&			mgr,
										const core::NodeAddress& 	expr, 
										const TmpVarMap& 			tmpVarMap=TmpVarMap(),
										bool 						final=true) 
	{
		return getImmediateAccess(mgr, UnifiedAddress(expr), tmpVarMap, final);
	}

	inline AccessPtr getImmediateAccess(core::NodeManager& 		mgr,
										const cfg::Address& 	expr, 
										const TmpVarMap& 		tmpVarMap=TmpVarMap(), 
										bool					final=true) 
	{
		return getImmediateAccess(mgr, UnifiedAddress(expr), tmpVarMap, final);
	}


	// Given an expression, this function is going to scan all the sub expression return a vector of
	// access which was found inside the given expression 
	AccessVector getAccesses(core::NodeManager& mgr, 
							 const UnifiedAddress& expr, 
							 const TmpVarMap& tmpVarMap=TmpVarMap());


	// Given two access descriptor states whether they are equal excluding the root node.
	bool equalPath(const AccessPtr& lhs, const AccessPtr& rhs);

} // end access namespace 
} // end analysis namespace 
} // end insieme namespace 


namespace std {

	std::ostream& operator<<(std::ostream& out, const insieme::analysis::access::AccessPtr& access);

} // end std namesapce 
