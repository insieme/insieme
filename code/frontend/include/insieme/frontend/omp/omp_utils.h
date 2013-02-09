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
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/ir_builder.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace frontend {
namespace omp {

struct GlobalRequiredAnnotation : public core::NodeAnnotation { 
	const static string name;
	const static utils::StringKey<GlobalRequiredAnnotation> key; 
	virtual const utils::AnnotationKeyPtr getKey() const {
		return &key;
	}
	virtual const std::string& getAnnotationName() const {
		return name;
	}
	virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const;
};

// Utility for marking paths that require OMP globals and gathering the required globals
core::StructExpr::Members markGlobalUsers(const core::ProgramPtr& prog);

// Utility for passing global to functions that require it, and replacing literals with global accesses
class GlobalMapper : public core::NodeMapping {
	core::NodeManager& nodeMan;
	core::IRBuilder build;
	core::VariablePtr curVar;
	bool startedMapping;

	/* Caching of generated lambdas */
	typedef member_function_trait<const core::NodePtr(GlobalMapper::*)(const core::LambdaExprPtr&)>::type FactoryType;
	insieme::utils::cache::PointerCache<core::LambdaExprPtr, core::NodePtr, FactoryType> cache;

public:
	GlobalMapper(core::NodeManager& nodeMan, const core::VariablePtr& global) 
			: nodeMan(nodeMan), build(nodeMan), curVar(global), startedMapping(false), cache(fun(*this, &GlobalMapper::mapLambdaExpr)) {
	}

protected:
	virtual const core::NodePtr mapElement(unsigned index, const core::NodePtr& ptr);
		
	const core::NodePtr mapCall(const core::CallExprPtr& call);
	const core::NodePtr mapBind(const core::BindExprPtr& bind);
	//const core::NodePtr mapJob(const core::JobExprPtr& bind);
	const core::NodePtr mapLambdaExpr(const core::LambdaExprPtr& lambdaExpr);
	const core::NodePtr mapLiteral(const core::LiteralPtr& literal);
};

} // namespace omp
} // namespace frontend
} // namespace insieme
