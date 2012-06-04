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

#include "insieme/machine_learning/feature_preconditioner.h"
#include "insieme/machine_learning/machine_learning_exception.h"

namespace insieme {
namespace ml {

/*
 * performs (((x - mean) / MAX(max - mean, mean - min) - (1 - lower) ) * (upper - lower)
 */
void FeaturePreconditioner::transformData(Array<double>& features) {
	if(prop.dim(0) != 4)
		throw MachineLearningException("Properties array has not been initialized before call to FeaturePreconditioner::transformData");

	size_t nFeatures = (features.ndim() > 1 ? features.dim(1) : features.dim(0));

	if(prop.dim(1) != nFeatures)
		throw MachineLearningException("Loaded property array does not have the same number of features as the pattern to classify");


	// FIXME add proper handling for feature access
	//auto accessFeatures = 0;//(features.ndim() > 1) ? ([&](size_t x, size_t y){ return features(x,y); }) : ([&](size_t x, size_t y){ return features(x); });

	double lower = prop(3,0), upper = prop(3,1);
	Array<double> divisor(nFeatures);
	// calculate the divisor to normalize each feature
	for(size_t f = 0; f < nFeatures; ++f) {
		divisor(f) = fmax(prop(0,f) - prop(1,f), prop(2,f) - prop(0,f));
	}

//	std::cout << features.dim(0) << " < " << prop.dim(1) << std::endl;
	double interval = (upper - lower) / 2;

	// normalize the data
	if(lower == -upper) {
		for(size_t i = 0; i < features.dim(0); ++i) {
			//FIXME make it prettier
			if(features.ndim() > 1) {
			for(size_t f = 0; f < nFeatures; ++f) {
				// avoid division by 0
				if(divisor(f) == 0)
					features(i,f) = 0.0;
				else
					features(i,f) = ((features(i,f) - prop(0,f)) / divisor(f)) * interval;
			}
			}else{
				if(divisor(i) == 0)
					features(i) = 0.0;
				else
					features(i) = ((features(i) - prop(0,i)) / divisor(i)) * interval;
			}
		}
	} else {
		double lift = lower + interval;
		for(size_t i = 0; i < features.dim(1); ++i) {
			if(features.ndim() > 1) {
			for(size_t f = 0; f < nFeatures; ++f) {
				// avoid division by 0
				if(divisor(f) == 0)
					features(i,f) = 0.0;
				else
					features(i,f) = ((features(i,f) - prop(0,f)) / divisor(f) * interval) + lift;
			}
			}else{
				if(divisor(i) == 0)
					features(i) = 0.0;
				else
					features(i) = ((features(i) - prop(0,i)) / divisor(i) * interval) + lift;
			}
		}
	}

//	std::cout << "!!!!!!! " << prop << std::endl;

}


/*
 * calculates the mean of column idx
 */
void FeaturePreconditioner::calcProp(Array<double>& features){
	size_t nFeatures = (features.ndim() > 1 ? features.dim(1) : 1);

	prop.resize(4u, nFeatures, false);

	// initialize variables
	for(size_t f = 0; f < nFeatures; ++f) {
		double tmp = features(0,f);
		prop(0,f) = tmp;
		prop(1,f) = tmp;
		prop(2,f) = tmp;

		// fill array to make valgrind happy
		prop(3,f) = 0.0;
	}

	// find sum, min and max
	for(size_t i = 1; i < features.dim(0); ++i) {
		for(size_t f = 0; f < nFeatures; ++f) {
			double tmp = features(i,f);
			prop(0,f) += tmp;
			if(prop(1,f) > tmp)
				prop(1,f) = tmp;
			if(prop(2,f) < tmp)
				prop(2,f) = tmp;
		}
	}

	// transform the sum to average
	for(size_t f = 0; f < nFeatures; ++f) {
		prop(0,f) /= features.dim(0);
	}
}


/*
 * normalizes the each column in the dataset and returns an array holding the means
 */
Array<double> FeaturePreconditioner::normalize(Array<double>& features, double lower, double upper) {
	calcProp(features);

	// append the lower and upper boundary as last line to the proppertie's array
	prop(3,0) = lower, prop(3,1) = upper;

	transformData(features);

	return prop;
}

} // end namespace ml
} // end namespace insieme
