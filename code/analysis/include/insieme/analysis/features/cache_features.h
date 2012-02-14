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
#include "insieme/analysis/features/feature.h"

namespace insieme {
namespace analysis {
namespace features {

	/**
	 * The model representing the cache within the simulation.
	 */
	class CacheModel {

	public:

		virtual ~CacheModel() {}

		virtual void access(long location, int size) =0;

		virtual TypePtr getFeatureType() const = 0;
		virtual Value getFeatureValue() const = 0;

	};

	class HitMissModel : public CacheModel {

		long hits;
		long misses;

	public:

		void reset() { hits = 0; misses = 0; }

		long getHits() const { return hits; }
		long getMisses() const { return misses; }

		long getAccesses() const { return hits + misses; }

		double getMissRatio() const { return misses/(double)(hits + misses); }
		double getHitRatio() const { return hits/(double)(hits + misses); }

		virtual TypePtr getFeatureType() const;
		virtual Value getFeatureValue() const;

	protected:

		void hit() { hits++; }
		void miss() { misses++; }
	};

	template<int LineSize, int NumLines>
	class DirectCacheModel : public HitMissModel {

		long cache[NumLines];

	public:

		DirectCacheModel() {
			reset();
		}

		void reset() {
			// clear stats
			HitMissModel::reset();

			// invalidate cache content
			for (int i=0; i<NumLines; i++) {
				cache[i] = -1;
			}
		}

		virtual void access(long location, int size) {

			int row = ( location / LineSize ) % NumLines;
			int col = location % LineSize;

			for (long pos = location; pos < location + size; pos++) {

				int base = pos - col;
				if (cache[row] == base) {
					hit();
				} else {
					miss();

					// update full cache line
					cache[row] = base;
				}

				col++;
				if (col > LineSize) {
					col = 0; row = ( row + 1 ) % NumLines;
				}
			}
		}

	};

	template<int LineSize, int NumSets, int Ways>
	class LRUCacheModel : public HitMissModel {

		struct Line {
			Line* newer;
			Line* older;
			long base;

			void reset() {
				base = -1;
			}
		};

		struct Set {
			Line* mru; Line* lru;
			Line lines[Ways];

			void touch(Line* line) {
				// check whether movement is necessary
				if (line == mru) {
					return;
				}

				// update lru
				if (lru == line) {
					lru = lru->newer;
				}

				// move given line to mru position

				// cut out line from list
				if (line->older) line->older->newer = line->newer;
				if (line->newer) line->newer->older = line->older;

				// insert line ad head of list
				mru->newer = line;
				line->older = mru;
				line->newer = NULL;

				// make line the new mru
				mru = line;
			}

			bool contains(long pos) {
				long base = pos - (pos % LineSize);
				for (int i=0; i<Ways; ++i) {
					if (lines[i].base == base) {
						touch(&lines[i]);
						return true;
					}
				}
				return false;
			}

			void load(long pos) {
				assert(!contains(pos) && "Should not be called if position is contained!");

				// replace LRU line
				int base = pos - (pos % LineSize);
				lru->base = base;

				// make LRU line MRU line
				touch(lru);
			}

			void reset() {
				mru = &lines[Ways-1];
				lru = &lines[0];

				for(int i=0; i<Ways; i++) {
					lines[i].reset();
					lines[i].newer = &lines[i+1];
					lines[i].older = &lines[i-1];
				}

				lines[0].older = NULL;
				lines[Ways-1].newer = NULL;
			}
		};

		Set cache[NumSets];

	public:

		LRUCacheModel() {
			reset();
		}

		void reset() {
			// clear stats
			HitMissModel::reset();

			// clear cache
			for (int i=0; i<NumSets; i++) {
				cache[i].reset();
			}
		}

		bool contains(Set& set, int col, long location) {
			for(int i=0; i<Ways; i++) {
				if (set.lines[col] == location) {
					return true;
				}
			}
			return false;
		}


		virtual void access(long location, int size) {

			int block = ( location / LineSize ) % NumSets;
			int col = location % LineSize;

			for (long pos = location; pos < location + size; pos++) {

				if (cache[block].contains(pos)) {
					// all fine
					hit();
				} else {
					// miss => replace
					miss();

					// load data
					cache[block].load(pos);
				}

				col++;
				if (col > LineSize) {
					col = 0; block = ( block + 1 ) % NumSets;
				}
			}
		}

	};

	template<int LineSize, int NumLines>
	class LRUCacheModel<LineSize, NumLines, 1> : public DirectCacheModel<LineSize, NumLines> {};



	typedef std::shared_ptr<CacheModel> CacheModelPtr;

	struct CacheModelFactory {
		virtual ~CacheModelFactory() {};
		virtual CacheModelPtr createModel() const = 0;
		TypePtr getFeatureType() const {
			return createModel()->getFeatureType();
		}
	};

	typedef std::shared_ptr<CacheModelFactory> CacheModelFactoryPtr;

	template<typename CacheModel>
	struct SimpleCacheModelFactory : public CacheModelFactory {
		virtual CacheModelPtr createModel() const {
			return std::make_shared<CacheModel>();
		}
	};

	template<typename CacheModel>
	CacheModelFactoryPtr createSimpleCacheModelFactory() {
		return std::make_shared<SimpleCacheModelFactory<CacheModel>>();
	}

	template<typename T, typename ... P>
	CacheModelPtr createCacheModel(P ... params) {
		return std::make_shared<T>(params ...);
	}


	void evalModel(const core::NodePtr& code, CacheModel& model);


	class CacheUsageFeature : public Feature {

		CacheModelFactoryPtr modelFactory;

	public:

		CacheUsageFeature(const string& name, const string& desc, const CacheModelFactoryPtr& factory)
			: Feature(true, name, desc, factory->getFeatureType()), modelFactory(factory) {}

		virtual Value evaluateFor(const core::NodePtr& code) const {
			CacheModelPtr model = modelFactory->createModel();
			evalModel(code, *model);
			return model->getFeatureValue();
		}

	};

	inline FeaturePtr createCacheFeature(const string& name, const string& desc, const CacheModelFactoryPtr& factory) {
		return std::make_shared<CacheUsageFeature>(name, desc, factory);
	}



} // end namespace features
} // end namespace analysis
} // end namespace insieme
