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

#include <stdexcept>
#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_types.h"

namespace insieme {
namespace analysis {
namespace features {

	/** 
	 * When the size of a type cannot be estimated (because containing arrays or
	 * vectors with a non-compile time specified size), this exception is thrown.
	 *
	 * The exception contains the estimated value of the type which caused the 
	 * exception.
	 */
	class UndefinedSize : public std::exception {
		// Type which caused the exception
		core::TypePtr type;
		// When the type cannot be precisely determined, its value is estimated
		// the estimated value is stored in the exception 
		unsigned estimation;

	public:
		UndefinedSize(const core::TypePtr& type, unsigned estimation) : 
			type(type), estimation(estimation) { }

		virtual const char* what() const throw () { return "Undefined type size"; }
		
		unsigned getEstimatedSize() const { return estimation; }
		
		const core::TypePtr& getType() const { return type; }

		virtual ~UndefinedSize() throw () { }
	};

	/**
	 * Estimates the number of bytes occupied by an instance of the given
	 * type. If the the size of the type cannot be determine, an exception is 
	 * thrown which contains the estimates size. Pointers are assumed to be
	 * 64bit values.
	 *
	 * @param type the type which's size should be estimated
	 * @param unknownContainerSize the size used for estimating the size of a container
	 * 			if its size can not be determined (e.g. array types or vector<X,'a> types).
	 * @return the estimated size of an instance of the given type
	 */
	unsigned getSizeInBytes(const core::TypePtr& type, unsigned unknownContainerSize = 100);

	/**
	 * Like the above method, the only difference is that this method will
	 * never throw the UndefinedSize exception, but instead will return the 
	 * estimated value. The estimation will assume a dense packing of structs 
	 * and arrays having a size of 100. Referenced objects are not considered. 
	 */
	unsigned getEstimatedSizeInBytes(const core::TypePtr& type, unsigned unknownContainerSize = 100);

	/**
	 * Obtains the offset of a member element within a struct. It is simply the accumulation
	 * of the size of all preceding memory locations.
	 *
	 * @param type the struct type containging the member
	 * @param member the name of the members which's offset should be obtained
	 * @return the offest of the given member in bytes
	 */
	unsigned getMemberOffsetInBytes(const core::StructTypePtr& type, const core::StringValuePtr& member);

} // end namespace features
} // end namespace analysis
} // end namespace insieme
