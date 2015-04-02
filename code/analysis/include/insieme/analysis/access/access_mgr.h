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

#include "insieme/analysis/access/access.h"
#include "insieme/analysis/access/access_class.h"

namespace insieme {
namespace analysis {
namespace access {

	/**
	 * The AccessManager takes care of managing and creating access classes 
	 */
	class AccessManager: public utils::Printable {

	public:
		typedef std::vector<AccessClassPtr> ClassVector;

	private:
		ClassVector classes;
		
		const CFG* 			cfg;
		const TmpVarMap& 	tmpVarMap;

	//	std::tuple<AccessClassPtr,bool>  
	//	classify(const AccessClassPtr& 				parent,  
	//			const AccessDecoratorPtr& 			subLevel, 
	//			const AccessPtr& 					currAccess);


		AccessClassPtr addClass(AccessClassPtr parent, const AccessPtr& access, const AccessDecoratorPtr& dec, 
				bool append_to_parent=true);

	public:

		typedef ClassVector::iterator 		iterator;
		typedef ClassVector::const_iterator const_iterator;

		AccessManager(const CFG* cfg = nullptr, const TmpVarMap& tmpVarMap = TmpVarMap()) : 
			cfg(cfg), 
			tmpVarMap(tmpVarMap) { }

		/**
		 * An access can belong to multiple classes, therefore when we look for the class 
		 * containing a particular access we may hit multiple classes
		 */
		AccessClassSet getClassFor(const AccessPtr& access, bool sub=false);

		AccessClassSet findClass(const AccessPtr& access) const;

		void printDotGraph(std::ostream& out) const;

		// Iterators 
		inline iterator begin() { return classes.begin(); }
		inline iterator end() { return classes.end(); }

		inline const_iterator begin() const { return classes.begin(); }
		inline const_iterator end() const { return classes.end(); }

		// Accessors 
		inline AccessClass& operator[](const size_t uid) { 
			assert(uid < classes.size() && "OutOfBounds array access");
			return *classes[uid]; 
		}

		inline const AccessClass& operator[](const size_t uid) const {
			assert(uid < classes.size() && "OutOfBounds array access");
			return *classes[uid]; 
		}

		inline size_t size() const { return classes.size(); }

		std::ostream& printTo(std::ostream& out) const;

	};

} // end access namespace 
} // end analysis namespace 
} // end insieme namespace
