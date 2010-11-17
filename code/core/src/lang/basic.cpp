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

#include "insieme/core/lang/basic.h"

#include "insieme/core/ir_parse.h"
#include "insieme/core/ast_builder.h"

namespace insieme {
namespace core {
namespace lang {

struct BasicGenerator::BasicGeneratorImpl {
	parse::IRParser parser;
	ASTBuilder build;

	#define TYPE(_id, _spec) \
	TypePtr ptr##_id;

	#define LITERAL(_id, _name, _spec) \
	LiteralPtr ptr##_id;

	#include "insieme/core/lang/lang.def"

	#undef TYPE
	#undef LITERAL

	// ----- extra material ---

	StatementPtr ptrNoOp;

	BasicGeneratorImpl(NodeManager& nm) : parser(nm), build(nm) { }
};

BasicGenerator::BasicGenerator(NodeManager& nm) : nm(nm), pimpl(new BasicGeneratorImpl(nm)) {
}
#define TYPE(_id, _spec) \
TypePtr BasicGenerator::get##_id() const { \
	if(!pimpl->ptr##_id) pimpl->ptr##_id = pimpl->parser.parseType(_spec); \
	return pimpl->ptr##_id; }; \
bool BasicGenerator::is##_id(const NodePtr& p) const { \
	return *p == *get##_id(); };

#define LITERAL(_id, _name, _spec) \
LiteralPtr BasicGenerator::get##_id() const { \
	if(!pimpl->ptr##_id) pimpl->ptr##_id = pimpl->build.literal(_name, pimpl->parser.parseType(_spec)); \
	return pimpl->ptr##_id; }; \
bool BasicGenerator::is##_id(const NodePtr& p) const { \
	return *p == *get##_id(); };

#include "insieme/core/lang/lang.def"

#undef TYPE
#undef LITERAL

// ----- extra material ---

StatementPtr BasicGenerator::getNoOp() const {
	if (!pimpl->ptrNoOp) {
		pimpl->ptrNoOp = pimpl->build.compoundStmt();
	}
	return pimpl->ptrNoOp;
}

bool BasicGenerator::isNoOp(const NodePtr& p) const {
	return *p == *getNoOp();
}

} // namespace lang
} // namespace core
} // namespace insieme
