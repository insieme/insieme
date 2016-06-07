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

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/opencl/opencl_entities.h"
#include "insieme/utils/id_generator.h"

namespace insieme {
namespace backend {
namespace opencl {

	using namespace insieme::core;

	class KernelTable;
	typedef Ptr<KernelTable> KernelTablePtr;

	class DataRequirementTable;
	typedef Ptr<DataRequirementTable> DataRequirementTablePtr;

	class KernelTable : public c_ast::CodeFragment {
		struct Entry {
			unsigned id;
			std::string source;
			std::string routine;
		};
		typedef std::shared_ptr<Entry> EntryPtr;

		const Converter& converter;
		std::map<unsigned, EntryPtr> impls;
		c_ast::CodeFragmentPtr declaration;

		static utils::SimpleIDGenerator<unsigned> idGenerator;
	public:
		KernelTable(const Converter& converter);
		static KernelTablePtr get(const Converter& converter);
		static unsigned getNextUnique();

		c_ast::CodeFragmentPtr getDeclaration() const;
		const c_ast::ExpressionPtr getTable() const;
		unsigned size() const;

		unsigned registerKernel(const ExpressionPtr& id, const ExpressionPtr& source, const ExpressionPtr& routine);
		std::ostream& printTo(std::ostream& out) const override;
	};
} // end namespace opencl
} // end namespace backend
} // end namespace insieme
