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

#include <array>

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace analysis {
namespace features {

	struct CacheUsage {
		long numMisses;
		long numHits;

		CacheUsage() : numMisses(0), numHits(0) {}

		void reset() {
			numMisses = 0; numHits = 0;
		};

		void hit() { numHits++; }
		void miss() { numMisses++; }
	};

	/**
	 * The model representing the cache within the simulation.
	 */
	class CacheModel {

	public:

		virtual ~CacheModel() {}

		virtual void access(long location, int size) =0;

	};

	template<
		int LineSize,
		int NumLines
	>
	class SimpleCacheModel : public CacheModel {

		long cache[NumLines][LineSize];

		long hits;
		long misses;

	public:

		SimpleCacheModel() {
			reset();
		}

		void reset() {
			// clear stats
			hits = 0;
			misses = 0;

			// invalidate cache content
			for (int i=0; i<NumLines; i++) {
				for (int j=0; j<LineSize; j++) {
					cache[i][j] = -1;
				}
			}
		}

		long getHits() const {
			return hits;
		}

		long getMisses() const {
			return misses;
		}

		virtual void access(long location, int size) {

			for (long pos = location; pos < location + size; pos++) {

				int row = ( pos / LineSize ) % NumLines;
				int col = pos % LineSize;

				if (cache[row][col] == pos) {
					hits++;
				} else {
					misses++;

					// update full cache line
					int base = pos - col;
					for (int i=0; i<LineSize; ++i) {
						cache[row][i] = base + i;
					}
				}
			}
		}

	};


	void evalModel(const core::NodePtr& code, CacheModel& model);


} // end namespace features
} // end namespace analysis
} // end namespace insieme
