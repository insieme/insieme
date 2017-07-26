/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/analysis/cba/common/preprocessing.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace analysis {
namespace cba {

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


		LANG_EXT_LITERAL(RefIsNull,    "cba_expect_null_ref", "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefNotNull,   "cba_expect_not_null_ref", "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefMaybeNull, "cba_expect_maybe_null_ref", "(ref<'a>)->unit");

		LANG_EXT_DERIVED(PtrIsNull,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_null_ref(ptr_to_ref(a));      "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrNotNull,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_not_null_ref(ptr_to_ref(a));      "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrMaybeNull,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_maybe_null_ref(ptr_to_ref(a));      "
			"  }                                               "
		)


		LANG_EXT_LITERAL(RefIsExtern,    "cba_expect_extern_ref", "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefNotExtern,   "cba_expect_not_extern_ref", "(ref<'a>)->unit");
		LANG_EXT_LITERAL(RefMaybeExtern, "cba_expect_maybe_extern_ref", "(ref<'a>)->unit");

		LANG_EXT_DERIVED(PtrIsExtern,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_extern_ref(ptr_to_ref(a));      "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrNotExtern,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_not_extern_ref(ptr_to_ref(a));      "
			"  }                                               "
		)

		LANG_EXT_DERIVED(PtrMaybeExtern,
			"  (a : ptr<'a>) -> unit {                         "
			"		cba_expect_maybe_extern_ref(ptr_to_ref(a));      "
			"  }                                               "
		)

	};

	core::ProgramPtr preProcessing(const core::ProgramPtr& prog) {
		const auto& ext = prog.getNodeManager().getLangExtension<CBAInputTestExt>();
		return core::transform::transformBottomUpGen(prog,
				[&] (const core::ExpressionPtr& expr)->core::ExpressionPtr {

					if (!expr.isa<core::LiteralPtr>() && !expr.isa<core::LambdaExprPtr>()) return expr;

					const string& name = (expr.isa<core::LiteralPtr>()) ?
							utils::demangle(expr.as<core::LiteralPtr>()->getStringValue()) :
							utils::demangle(expr.as<core::LambdaExprPtr>()->getReference()->getNameAsString()) ;


					if(name == "cba_expect_is_alias")       return ext.getPtrAreAlias();
					if(name == "cba_expect_may_alias")      return ext.getPtrMayAlias();
					if(name == "cba_expect_not_alias")      return ext.getPtrNotAlias();
					if(name == "cba_expect_undefined_ptr")  return ext.getPtrUndefined();
					if(name == "cba_expect_defined_ptr")    return ext.getPtrDefined();
					if(name == "cba_expect_single_ptr")     return ext.getPtrSingle();
					if(name == "cba_expect_not_single_ptr") return ext.getPtrNotSingle();
					if(name == "cba_expect_null_ptr")       return ext.getPtrIsNull();
					if(name == "cba_expect_not_null_ptr")   return ext.getPtrNotNull();
					if(name == "cba_expect_maybe_null_ptr") return ext.getPtrMaybeNull();
					if(name == "cba_expect_extern_ptr")       return ext.getPtrIsExtern();
					if(name == "cba_expect_not_extern_ptr")   return ext.getPtrNotExtern();
					if(name == "cba_expect_maybe_extern_ptr") return ext.getPtrMaybeExtern();
					return expr;
				},
				core::transform::globalReplacement
		);
	}

}
}
}
