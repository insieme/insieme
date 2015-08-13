/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Extension ----------------------------

	/**
	 * An extension covering two types to model boolean properties.
	 */
	class BooleanMarkerExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		BooleanMarkerExtension(core::NodeManager& manager)
			: core::lang::Extension(manager) {}

	public:

		LANG_EXT_TYPE_WITH_NAME(True, "true_marker", "t");

		LANG_EXT_TYPE_WITH_NAME(False, "false_marker", "f");

	};

	static inline bool isTrueMarker(const NodePtr& node) {
		return node && node->getNodeManager().getLangExtension<BooleanMarkerExtension>().isTrue(node);
	}

	static inline bool isFalseMarker(const NodePtr& node) {
		return node && node->getNodeManager().getLangExtension<BooleanMarkerExtension>().isFalse(node);
	}

	static inline bool isValidBooleanMarker(const TypePtr& type) {
		return type.isa<TypeVariablePtr>() || isTrueMarker(type) || isFalseMarker(type);
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme
