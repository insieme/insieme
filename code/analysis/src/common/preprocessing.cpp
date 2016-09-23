/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/common/preprocessing.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace analysis {

	class CBAInputTestExt : public core::lang::Extension {

		/**
		* Allow the node manager to create instances of this class.
		*/
		friend class core::NodeManager;

		/**
		* Creates a new instance based on the given node manager.
		*/
		CBAInputTestExt(core::NodeManager& manager) : core::lang::Extension(manager) {}

	public:

		// this extension is based upon the symbols defined by the pointer module
		IMPORT_MODULE(core::lang::PointerExtension);

		LANG_EXT_LITERAL(RefAreAlias, "cba_expect_ref_are_alias", "(ref<'a>,ref<'a>)->unit");
		LANG_EXT_LITERAL(RefMayAlias, "cba_expect_ref_may_alias", "(ref<'a>,ref<'a>)->unit");
		LANG_EXT_LITERAL(RefNotAlias, "cba_expect_ref_not_alias", "(ref<'a>,ref<'a>)->unit");

		LANG_EXT_DERIVED(PtrAreAlias,
			"  (a : ptr<'a>, b : ptr<'a>) -> unit {                         "
			"		cba_expect_ref_are_alias(ptr_to_ref(a),ptr_to_ref(b));  "
			"  }                                                            "
		)

		LANG_EXT_DERIVED(PtrMayAlias,
			"  (a : ptr<'a>, b : ptr<'a>) -> unit {                         "
			"		cba_expect_ref_may_alias(ptr_to_ref(a),ptr_to_ref(b));  "
			"  }                                                            "
		)

		LANG_EXT_DERIVED(PtrNotAlias,
			"  (a : ptr<'a>, b : ptr<'a>) -> unit {                         "
			"		cba_expect_ref_not_alias(ptr_to_ref(a),ptr_to_ref(b));  "
			"  }                                                            "
		)

		LANG_EXT_LITERAL(RefUndefined, "cba_expect_undefined_ref",  "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefDefined,   "cba_expect_defined_ref",    "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefSingle,    "cba_expect_single_ref",     "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefNotSingle, "cba_expect_not_single_ref", "(ref<'a>)->unit");

		LANG_EXT_DERIVED(PtrUndefined,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_undefined_ref(ptr_to_ref(a));   "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrDefined,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_defined_ref(ptr_to_ref(a));     "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrSingle,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_single_ref(ptr_to_ref(a));      "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrNotSingle,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_not_single_ref(ptr_to_ref(a));      "
			"  }                                               "
		)
	};

	core::ProgramPtr preProcessing(const core::ProgramPtr& prog) {
		const auto& ext = prog.getNodeManager().getLangExtension<CBAInputTestExt>();
		return core::transform::transformBottomUpGen(prog,
				[&] (const core::LiteralPtr& lit)->core::ExpressionPtr {
					const string& name = utils::demangle(lit->getStringValue());
					if(name == "cba_expect_is_alias")       return ext.getPtrAreAlias();
					if(name == "cba_expect_may_alias")      return ext.getPtrMayAlias();
					if(name == "cba_expect_not_alias")      return ext.getPtrNotAlias();
					if(name == "cba_expect_undefined_ptr")  return ext.getPtrUndefined();
					if(name == "cba_expect_defined_ptr")    return ext.getPtrDefined();
					if(name == "cba_expect_single_ptr")     return ext.getPtrSingle();
					if(name == "cba_expect_not_single_ptr") return ext.getPtrNotSingle();
					return lit;
				},
				core::transform::globalReplacement
		);
	}

}
}
