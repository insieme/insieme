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

#include "insieme/frontend/extensions/long_long_extension.h"

#include "insieme/core/ir.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/assert.h"

 namespace insieme {
 namespace frontend {

	insieme::core::ProgramPtr LongLongExtension::IRVisit(insieme::core::ProgramPtr& prog){

		// replace all long long literals by int8 or uint8
		//auto literalTypeFixer = core::transform::makeCachedLambdaMapper([](const core::NodePtr& node)-> core::NodePtr{
		//			if (core::LiteralPtr lit = node.isa<core::LiteralPtr>()){
		//				if (core::analysis::isLongLong(lit->getType())){
		//					core::IRBuilder builder (node->getNodeManager());
		//					if (core::analysis::isSignedLongLong(lit->getType()))
		//						return builder.literal(lit->getValue(), builder.getLangBasic().getInt8());
		//					else
		//						return builder.literal(lit->getValue(), builder.getLangBasic().getUInt8());
		//				}
		//			}
		//			return node;
		//		});
		//prog = literalTypeFixer.map(prog);

		// remove all superfluous casts
		auto castRemover = core::transform::makeCachedLambdaMapper([](const core::NodePtr& node)-> core::NodePtr{
					if (core::CallExprPtr call = node.isa<core::CallExprPtr>()){
						core::IRBuilder builder (node->getNodeManager());
						const auto& ext = node->getNodeManager().getLangExtension<core::lang::IRppExtensions>();

						// TO
						if (core::analysis::isCallOf(call, ext.getLongToLongLong()))  return call[0];
						if (core::analysis::isCallOf(call, ext.getULongToULongLong())) return call[0];

						// From
						if (core::analysis::isCallOf(call, ext.getLongLongToLong()))  return call[0];
						if (core::analysis::isCallOf(call, ext.getULongLongToULong())) return call[0];
						
						// betwen
						if (core::analysis::isCallOf(call, ext.getLongLongToULongLong()))
							return builder.callExpr(builder.getLangBasic().getUInt8(), builder.getLangBasic().getSignedToUnsigned(), toVector (call[0], builder.getIntParamLiteral(8)));
						if (core::analysis::isCallOf(call, ext.getULongLongToLongLong()))
							return builder.callExpr(builder.getLangBasic().getInt8(), builder.getLangBasic().getUnsignedToInt(), toVector (call[0], builder.getIntParamLiteral(8)));
					}
					return node;
				});
		prog = castRemover.map(prog);

		// finaly, substitute any usage of the long long types
		core::IRBuilder builder (prog->getNodeManager());
		core::TypePtr longlongTy = builder.structType(toVector( builder.namedType("longlong_val", builder.getLangBasic().getInt8()))); 
		core::TypePtr ulonglongTy = builder.structType(toVector( builder.namedType("longlong_val", builder.getLangBasic().getUInt8()))); 
		core::NodeMap replacements;
		replacements [ longlongTy ] = builder.getLangBasic().getInt8();
		replacements [ ulonglongTy ] = builder.getLangBasic().getUInt8();
		prog = core::transform::replaceAllGen (prog->getNodeManager(), prog, replacements, false);

		return prog;
	}

} // frontend
} // insieme
