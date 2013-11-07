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


#include "insieme_tasks.h"


namespace insieme{
namespace utils{



/**
 * wrap class for reductionr:
 *  a reduction is defined over a collection, we operate over an input type, and we have to
 *  compute a single unique output. 
 *  in order to use, we need to fullfill some requirements
 *  -  (T, S) - > S  is the prototype of the isolated operation to perform over a sigle element,
 *  -  this function needs to be provided of an identity element over the operator.
 *  -  the input data will be a collection of data to be reduced
 *  -  the output has to be a single value of type S
 */

	template <typename T, typename S>
	class ReductionTask : public TaskBase { 

		typedef typename std::vector<T> storage_t;
		typedef typename std::vector<T>::const_iterator iterator;


		const storage_t& collection;
		S& returnValue;
		const S& identityValue;

		typedef std::function<S(S, T)> fun_t;
		fun_t fun;

		public:

		ReductionTask (const storage_t& input, S& ret, const  S& identity, fun_t f)
			: collection(input), returnValue(ret), identityValue(identity), fun(f)
		{ }

		/** 
		 * this is the implementation of the the reduction task, 
		 * 	- easy to begin with, tree reduction
		 */
		virtual void operator()(){


			// matrix reduction, create a matrix with same rows as workers
			//	an undefined number of rows will perform operation on one extra element
			//	to map non divisivible workloads
			//
			//		<--------
			//		<--------
			//		<-------
			//		<-------
			//
			// after this we reduce the first colum
			//
			// 		^-------
			// 		|-------
			// 		|-------
			// 		|----
			//
			// 	result is in the 0,0 possition



			unsigned workers = TaskManager::getNumWorkers();
			unsigned items   = collection.size();
			if (workers > items) workers = items;

			float div = (float)items/(float)workers;
			unsigned quota   =  items/workers;
			float leftovers =  ceil((div-quota) * workers);

			std::vector<S> result (workers, identityValue);
			auto rowReduce = [this, &result]( iterator it,  iterator end, unsigned i) {
				for (; it != end; ++it)
					result[i] = std::move(fun(result[i], *it));
			};

			auto matrixReduce = task();
			iterator last = collection.begin();
			for(unsigned i=0; i<workers; ++i){

				iterator it = last;
				iterator end= it;
				if (quota*(i+1) >=  items) end = collection.end();
				else {
					end+=quota;
					if (i < leftovers) end++;
				}
				last = end;

				//auto tn = task< decltype(rowReduce), std::function<void(iterator, iterator, unsigned)>, iterator, iterator, unsigned>  (rowReduce, it, end, i);
				auto tn = task  (rowReduce, it, end, i);
				tn >> matrixReduce;
			}
			matrixReduce();

			returnValue = identityValue;
			for (const auto& cur: result){
				returnValue = std::move(fun(returnValue, cur));
			}

//			unsigned size = collection.size();
//			if (size%2 ==1) size++;
//			std::vector<S> result(size, identityValue);
//
//			auto loadWrap = [this, &result] (unsigned i, unsigned j){
//				result[i] = this->fun(this->collection[i], result[j]);
//			};
//
//			std::cout << " == LOAD: " << std::endl;
//			auto loadIn = task();
//			for (unsigned i=0; i < collection.size(); ++i){
//				auto tn = task(loadWrap, i, i);
//				tn >> loadIn;
//			}
//			loadIn();
//
//		std::cout << " ==================================== " << std::endl;
//		for (const auto& cur: result) std::cout << cur << " == " << std::endl;
//		std::cout << " ==================================== " << std::endl;
//
//			auto funWrap = [this, &result] (unsigned i, unsigned j){
//				result[i] = std::move(this->fun(result[i], result[j]));
//			};
//			std::cout << " ==  reduce: " << std::endl;
//			unsigned offset =1;
//			while (offset  <  size){
//
//			std::cout << " == level: " << std::endl;
//				auto level = task();
//				for (unsigned j= 0; j< size; j+=(offset*2)){
//					auto tn = task(funWrap, j, j+offset);
//					tn >> level;
//				}
//				level();
//			
//				// increase offset
//				offset *= 2;
//			}
//
//			returnValue = std::move(result[0]);

		}
	};


} // namespace
} // namespace



















