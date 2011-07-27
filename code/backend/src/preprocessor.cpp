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

#include "insieme/backend/preprocessor.h"

#include "insieme/backend/ir_extensions.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/ast_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"

namespace insieme {
namespace backend {


	core::NodePtr PreProcessingSequence::preprocess(core::NodeManager& manager, const core::NodePtr& code) {

		// start by copying code to given target manager
		core::NodePtr res = manager.get(code);

		// apply sequence of pre-processing steps
		for_each(preprocessor, [&](const PreProcessorPtr& cur) {
			res = cur->preprocess(manager, res);
		});

		// return final result
		return res;
	}


	// ------- concrete pre-processing step implementations ---------

	core::NodePtr NoPreProcessing::preprocess(core::NodeManager& manager, const core::NodePtr& code) {
		// just copy to target manager
		return manager.get(code);
	}



	// --------------------------------------------------------------------------------------------------------------
	//      ITE to lazy-ITE Convertion
	// --------------------------------------------------------------------------------------------------------------

	class ITEConverter : public core::transform::CachedNodeMapping {

		const core::LiteralPtr ITE;
		const IRExtensions extensions;

	public:

		ITEConverter(core::NodeManager& manager) :
			ITE(manager.basic.getIfThenElse()),  extensions(manager) {};

		/**
		 * Searches all ITE calls and replaces them by lazyITE calls. It also is aiming on inlining
		 * the resulting call.
		 */
		const core::NodePtr resolveElement(const core::NodePtr& ptr) {
			// do not touch types ...
			if (ptr->getNodeCategory() == core::NC_Type) {
				return ptr;
			}

			// apply recursively - bottom up
			core::NodePtr res = ptr->substitute(ptr->getNodeManager(), *this, true);

			// check current node
			if (!core::analysis::isCallOf(res, ITE)) {
				// no change required
				return res;
			}

			// exchange ITE call
			core::ASTBuilder builder(res->getNodeManager());
			core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(res);
			const vector<core::ExpressionPtr>& args = call->getArguments();
			res = builder.callExpr(extensions.lazyITE, args[0], evalLazy(args[1]), evalLazy(args[2]));

			// migrate annotations
			core::transform::utils::migrateAnnotations(ptr, res);

			// done
			return res;
		}

	private:

		/**
		 * A utility method for inlining the evaluation of lazy functions.
		 */
		core::ExpressionPtr evalLazy(const core::ExpressionPtr& lazy) {

			core::NodeManager& manager = lazy->getNodeManager();

			core::FunctionTypePtr funType = core::dynamic_pointer_cast<const core::FunctionType>(lazy->getType());
			assert(funType && "Illegal lazy type!");

			// form call expression
			core::CallExprPtr call = core::CallExpr::get(manager, funType->getReturnType(), lazy, toVector<core::ExpressionPtr>());
			return core::transform::tryInlineToExpr(manager, call);
		}
	};



	core::NodePtr IfThenElseInlining::preprocess(core::NodeManager& manager, const core::NodePtr& code) {
		// the converter does the magic
		ITEConverter converter(manager);
		return converter.map(code);
	}



	// --------------------------------------------------------------------------------------------------------------
	//      PreProcessor InitZero convert => replaces call by actual value
	// --------------------------------------------------------------------------------------------------------------

	class InitZeroReplacer : public core::transform::CachedNodeMapping {

		const core::LiteralPtr initZero;
		core::NodeManager& manager;
		const core::lang::BasicGenerator& basic;

	public:

		InitZeroReplacer(core::NodeManager& manager) :
			initZero(manager.basic.getInitZero()), manager(manager), basic(manager.getBasicGenerator()) {};

		/**
		 * Searches all ITE calls and replaces them by lazyITE calls. It also is aiming on inlining
		 * the resulting call.
		 */
		const core::NodePtr resolveElement(const core::NodePtr& ptr) {
			// do not touch types ...
			if (ptr->getNodeCategory() == core::NC_Type) {
				return ptr;
			}

			// apply recursively - bottom up
			core::NodePtr res = ptr->substitute(ptr->getNodeManager(), *this, true);

			// check current node
			if (core::analysis::isCallOf(res, initZero)) {
				// replace with equivalent zero value
				core::NodePtr zero = getZero(static_pointer_cast<const core::Expression>(res)->getType());

				// update result if zero computation was successfull
				res = (zero)?zero:res;
			}

			// no change required
			return res;
		}

	private:

		/**
		 * Obtains an expression of the given type representing zero.
		 */
		core::ExpressionPtr getZero(const core::TypePtr& type) {

			// if it is an integer ...
			if (basic.isInt(type)) {
				return core::Literal::get(manager, type, "0");
			}

			// if it is a real ..
			if (basic.isReal(type)) {
				return core::Literal::get(manager, type, "0.0");
			}

			// if it is a struct ...
			if (type->getNodeType() == core::NT_StructType) {

				// extract type and resolve members recursively
				core::StructTypePtr structType = static_pointer_cast<const core::StructType>(type);

				core::StructExpr::Members members;
				for_each(structType->getEntries(), [&](const core::StructType::Entry& cur) {
					members.push_back(std::make_pair(cur.first, getZero(cur.second)));
				});

				return core::StructExpr::get(manager, members);
			}

			// fall-back => no default initalization possible
			return core::ExpressionPtr();

		}

	};


	core::NodePtr InitZeroSubstitution::preprocess(core::NodeManager& manager, const core::NodePtr& code) {
		// the converter does the magic
		InitZeroReplacer converter(manager);
		return converter.map(code);
	}


} // end namespace backend
} // end namespace insieme
