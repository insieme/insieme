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

namespace insieme {
namespace ml {

/*
 * performs (((x - mean) / MAX(max - mean, mean - min) - (1 - lower) ) * (upper - lower)
 */
void FeaturePreconditioner::applyNormalize(Array<double>& prop, double lower, double upper) {
	Array<double> divisor(data.dim(1));
	// calculate the divisor to normalize each feature
	for(size_t f = 0; f < data.dim(1); ++f) {
		divisor(f) = fmax(prop(0,f) - prop(1,f), prop(2,f) - prop(0,f));
	}

	double interval = (upper - lower) / 2;

	// normalize the data
	if(lower == -upper) {
		for(size_t i = 0; i < data.dim(0); ++i) {
			for(size_t f = 0; f < data.dim(1); ++f) {
				data(i,f) = ((data(i,f) - prop(0,f)) / divisor(f)) * interval;
			}
		}
	} else {
		double lift = lower + interval;
		for(size_t i = 0; i < data.dim(0); ++i) {
			for(size_t f = 0; f < data.dim(1); ++f) {
				data(i,f) = ((data(i,f) - prop(0,f)) / divisor(f) * interval) + lift;
			}
		}
	}
}


/*
 * calculates the mean of column idx
 */
void FeaturePreconditioner::calcProp(Array<double>& prop){
	// initialize variables
	for(size_t f = 0; f < data.dim(1); ++f) {
		double tmp = data(0,f);
		prop(0,f) = tmp;
		prop(1,f) = tmp;
		prop(2,f) = tmp;
	}

	// find sum, min and max
	for(size_t i = 1; i < data.dim(0); ++i) {
		for(size_t f = 0; f < data.dim(1); ++f) {
			double tmp = data(i,f);
			prop(0,f) += tmp;
			if(prop(1,f) > tmp)
				prop(1,f) = tmp;
			if(prop(2,f) < tmp)
				prop(2,f) = tmp;
		}
	}

	// transform the sum to average
	for(size_t f = 0; f < data.dim(1); ++f) {
		prop(0,f) /= data.dim(0);
	}
}


/*
 * normalizes the each column in the dataset and returns an array holding the means
 */
Array<double> FeaturePreconditioner::normalize(double lower, double upper) {
	Array<double> prop(3, data.dim(1));

	calcProp(prop);

	applyNormalize(prop, lower, upper);

	return prop;
}

} // end namespace ml
} // end namespace insieme
