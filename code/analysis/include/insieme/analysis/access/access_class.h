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

#include <set>

#include "insieme/analysis/access/access.h"

namespace insieme {
namespace analysis {
namespace access {

	// Forward declaration for the Manager 
	class AccessManager;

	// Forward declaration of a class 
	class AccessClass;

	typedef std::shared_ptr<AccessClass> AccessClassPtr;
	typedef std::weak_ptr<AccessClass>   AccessClassWPtr;

	typedef std::set<AccessClassPtr, compare_target<AccessClassPtr>> AccessClassSet; 

	/** 
	 * An access class is a set of accesses which refer to the same memory location. In case of
	 * R-Values an access refers to the actual value. Important to notice that access classes are
	 * specific to a program point (represented by a CFG blok)
	 *
	 * An access can refer to larger section of memory (in case of array accesses inside
	 * loopbounds), in that case a class contains all the accesses which may have a conflict.
	 *
	 * Accesses classes are meant to be used in DF analysis and be stored into sets, which means
	 * that they provide a partial order. 
	 */
	class AccessClass : public utils::Printable {

		std::reference_wrapper<const AccessManager> mgr;

		size_t uid;

	public:
		typedef std::vector<AccessPtr> AccessVector;

	private:
		/**
		 * Stores the accesses which refer to a memory area
		 */
		AccessVector accesses;

		friend class insieme::analysis::access::AccessManager;

	public:

		typedef std::tuple<AccessClassWPtr, AccessDecoratorPtr> Dependence;

		typedef std::vector<Dependence> SubClasses;

	private:
		/** 
		 * List of dependencies to sub-classes 
		 */
		SubClasses subClasses;

		/**
		 * Reference to the parent class 
		 */
		AccessClassWPtr parentClass;


		/** 
		 * AccessClass instances can only be created by the AccessMaanger class
		 */
		AccessClass(
				const std::reference_wrapper<const AccessManager>& mgr, 
				size_t uid, 
				const AccessClassWPtr parent = AccessClassWPtr()
		) : mgr(mgr), 
			uid(uid), 
			parentClass(parent) {  }

	public:

		AccessClass(const AccessClass& other): 
			mgr(other.mgr), 
			uid(other.uid), 
			accesses(other.accesses), 
			parentClass(other.parentClass) { }

		AccessClass(AccessClass&& other) : 
			mgr(other.mgr), 
			uid(other.uid), 
			accesses(std::move(other.accesses)),
			parentClass(std::move(other.parentClass)) { }

		AccessClass& storeAccess(const AccessPtr& access) {
			/** 
			 * Makes sure the access is not already in this class
			 */
			assert(!contains(access) && "Access is already present in this class");
				accesses.push_back(access); 
			return *this;
		}

		/** 
		 * Look for an access inside this class, the lookup is implemented by comparing for equivalence
		 * between accesses 
		 */
		bool contains(const AccessPtr& access) {
			auto fit = std::find_if( accesses.begin(), accesses.end(), 
							std::bind(equal_target<AccessPtr>(), std::placeholders::_1, access) );
			return fit != accesses.end();
		}

		/** 
		 * Return the unique identifier used to identify this access class.
		 *
		 * Comparison based on identifier is valid only within the same access manager.
		 */
		inline size_t getUID() const { return uid; }

		inline const AccessVector& getAccesses() const { return accesses; }

		inline void setParentClass(const AccessClassPtr& parent) { 
			this->parentClass = parent; 
		}

		const AccessClassPtr getParentClass() const {
			return parentClass.lock();
		}

		inline void addSubClass(const Dependence& dep) {
			subClasses.push_back(dep);
		}

		const SubClasses& getSubClasses() const { return subClasses; }
		SubClasses& getSubClasses() { return subClasses; }

		inline bool operator<(const AccessClass& other) const { 
			return uid < other.uid;
		}

		inline bool operator==(const AccessClass& other) const { 
			if (this == &other) { return true; }

			// check if the ID is the same and was generated by the same manager 
			return &mgr.get() == &other.mgr.get() && uid == other.getUID();
		}

		inline bool operator!=(const AccessClass& other) const {
			return !(*this == other);
		}

		std::ostream& printTo(std::ostream& out) const;

		inline AccessVector::const_iterator begin() const { return accesses.begin(); }
		inline AccessVector::const_iterator end() const { return accesses.end(); }

		inline size_t size() const { return accesses.size(); }

	};


	/** 
	 * Return the vector of addresses which are not temporary variable and therefore it returns
	 * addresses which exists only outside the CFG
	 */
	std::set<core::ExpressionAddress> 
	extractRealAddresses(const AccessClass& cl, const TmpVarMap& tmpVarMap = TmpVarMap());

	void addSubClasses(const AccessClassPtr& thisClass, AccessClassSet& collect);

	AccessClassSet getConflicting(const AccessClassSet& classes);

} // end access namespace 
} // end analysis namespace 
} // end insieme namespace 

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const insieme::analysis::access::AccessClassPtr& accClass) {
		return out << accClass->getUID(); 
	}

} // end std namespace 
