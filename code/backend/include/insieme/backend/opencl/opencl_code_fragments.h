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
