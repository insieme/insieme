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

#include "insieme/core/lang/extension.h"

#include "insieme/core/encoder/encoder.h"
#include "insieme/core/encoder/pointer_maps.h"
#include "insieme/core/encoder/tuples.h"

#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace lang {

class AsmStmtExtension : public core::lang::Extension {

	/**
	 * Allow the node manager to create instances of this class.
	 */
	friend class core::NodeManager;

	/**
	 * Creates a new instance based on the given node manager.
	 */
	AsmStmtExtension(core::NodeManager& manager)
			: core::lang::Extension(manager) {}


public:

	/**
	 * wrapper function to trigger the translation of this structure
	 */
	LANG_EXT_LITERAL(AsmStmt, "__insieme_asm", "('a) -> unit");

};


struct AsmStmtWrapper : public utils::Printable {

	bool _volatile;

	std::string asmString;

	typedef std::vector<std::pair<std::string, core::ExpressionPtr> > operandsList;
	operandsList outputs;
	operandsList inputs;

	std::vector<std::string> clobbers;

	/**
	 *  constructs an asm stmt wrapper to encode it throw the compiler pipeline
	 */
	AsmStmtWrapper (const std::string& asmString, bool volatil = false)
		: _volatile(volatil), asmString(asmString)
	{ }

	/**
	 * each output in the asm wrapper is a pair string:expression
	 */
	void addOutput (const std::string& str, const core::ExpressionPtr& ptr){
		outputs.push_back({str, ptr});
	}
	/**
	 * each input in the asm wrapper is a pair string:expression
	 */
	void addInput (const std::string& str, const core::ExpressionPtr& ptr){
		inputs.push_back({str, ptr});
	}
	/*
	 *  clobbers are strings
	 */
	void addClobber (const std::string& str){
		clobbers.push_back(str);
	}

	std::ostream& printTo (std::ostream& out) const{

		out << " ASM STMT: \n";
		if (_volatile) out << "\tvolatile ";
		out << "{ " << asmString <<  " } \n: ";
		for (auto& cur : outputs){
			out << "\"" << cur.first << "\"(" << cur.second << ") ,";
		}
		out << "\n : ";
		for (auto& cur : inputs){
			out << "\"" << cur.first << "\"(" << cur.second << "), ";
		}
		out << "\n : ";
		for (auto& cur : clobbers){
			out << "\"" << cur << "\"";
		}
		out << "\n";
		return out;
	}

};



AsmStmtWrapper fromIr(const core::ExpressionPtr& expr){
	AsmStmtWrapper wrap("");
	typedef decltype (std::make_tuple(wrap._volatile, wrap.asmString, wrap.outputs, wrap.inputs, wrap.clobbers)) tuple_type;
	tuple_type tmp = core::encoder::toValue<tuple_type> (expr);

	wrap._volatile = std::get<0>(tmp);
	wrap.asmString = std::get<1>(tmp);

	for (auto& cur : std::get<2>(tmp))
		wrap.addOutput(cur.first, cur.second);
	for (auto& cur : std::get<3>(tmp))
		wrap.addInput(cur.first, cur.second);
	for (auto& cur : std::get<4>(tmp))
		wrap.addClobber(cur);
	return wrap;
}

core::ExpressionPtr toIR(core::NodeManager& mgr,const AsmStmtWrapper& wrap){
	core::ExpressionPtr expr = core::encoder::toIR(mgr, std::make_tuple(wrap._volatile, wrap.asmString, wrap.outputs, wrap.inputs, wrap.clobbers));
	core::IRBuilder build(mgr);
	return build.callExpr(mgr.getLangExtension<AsmStmtExtension>().getAsmStmt(), expr);
}


}
}
}
