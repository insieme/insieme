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

namespace insieme {
namespace analysis {
namespace access {

	template <class RetTy>
	struct AccessVisitor {

		virtual RetTy visitBaseAccess(const BaseAccessPtr& access) = 0;

		virtual RetTy visitDecorator(const AccessDecoratorPtr& access) = 0;
		virtual RetTy visitDeref(const DerefPtr& access) = 0;
		virtual RetTy visitMember(const MemberPtr& access) = 0;
		virtual RetTy visitSubscript(const SubscriptPtr& access) = 0;

		virtual RetTy visit(const AccessPtr& access) = 0;

		virtual ~AccessVisitor() { }
	};


	template <class RetTy>
	struct RecAccessVisitor : public AccessVisitor<RetTy> {

		RetTy visitBaseAccess(const BaseAccessPtr& access) = 0;

		RetTy visitDecorator(const AccessDecoratorPtr& access) {

			switch(access->getType()) {
			case AccessType::AT_DEREF: 		return visitDeref(cast<Deref>(access));
			case AccessType::AT_MEMBER:		return visitMember(cast<Member>(access));
			case AccessType::AT_SUBSCRIPT:	return visitSubscript(cast<Subscript>(access));

			default:
				assert(false && "Type of decorator not recognized");
			}
			return RetTy();
		}

		RetTy visitDeref(const DerefPtr& access) {
			return visitDecoratorImpl(access);
		}

		RetTy visitMember(const MemberPtr& access) {
			return visitDecoratorImpl(access);
		}

		RetTy visitSubscript(const SubscriptPtr& access) {
			return visitDecoratorImpl(access);
		}

		RetTy visit(const AccessPtr& access) {
		
			assert(access && "Visiting invalid access pointer");

			if (access->getType() == AccessType::AT_BASE) {
				// handles base-access nodes 
				return visitBaseAccess(cast<BaseAccess>(access));
			}

			// handles decorators, by redirecting the dispatch to the visitDecorator method 
			return visitDecorator(cast<AccessDecorator>(access));
		}

	private:

		RetTy visitDecoratorImpl(const AccessDecoratorPtr& access) {
			visit(access->getSubAccess());
			return RetTy();
		}

	};

} // end access namespace 
} // end analyiss namespace 
} // end insieme namespace

