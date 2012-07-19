#ifdef INSIEME
#include "ocl_device.h"
#endif

float2 blend(float factor, float2 value1, float2 value2) {
	float factor2 = 1.0f - factor;
	float2 tmp;
	tmp.x = factor2 * value1.x + factor * value2.x;
	tmp.y = factor2 * value1.y + factor * value2.y;
	return tmp;
}

#pragma insieme mark
__kernel
void computeFlowMap(  __global float2 *data, int width, float2 dataOrigin, float2 dataCellSize, __global float *timesteps, int numTimesteps, float startTime, float advectionTime, __global float2 *output, int num_elements) {
        int gid = get_global_id(0);
        if (gid >= num_elements) return;
        int tx = gid % width; 
        int ty = gid / width; 

	const unsigned int numSteps = 1000;
	float timestep = advectionTime / numSteps;

	float2 pos;
	pos.x = dataOrigin.x + tx * dataCellSize.x;
	pos.y = dataOrigin.y + ty * dataCellSize.y;

	for (unsigned int step = 0; step < numSteps; ++step) {
		float currentTime = startTime + step * timestep;
	  	
		// previous time index
		int prevIndex = -1;

		for (unsigned int previous = 0; previous < numTimesteps; ++previous) {
			if (timesteps[previous] <= currentTime) 
				prevIndex = previous;
		}
	  
		if (prevIndex < 0 || prevIndex > numTimesteps - 2) {
			output[gid].x = 0;
			output[gid].y = 0;
			return;
		}

		// next time index
		int nextIndex = prevIndex + 1;
	  
		float2 interpolatedPrev;
		float2 interpolatedNext;

		float2 posW;
		posW.x = (pos.x - dataOrigin.x) / dataCellSize.x;
		posW.y = (pos.y - dataOrigin.y) / dataCellSize.y;

		// posXi,posYi is integral coordinate of "upper left corner"
		float posX = floor(posW.x);
		float posY = floor(posW.y);

		posX = clamp(posX, 0.0f, (float)(width - 2));
		posY = clamp(posY, 0.0f, (float)(width - 2));
  
		// get local coordinates
		float2 lpos;
		lpos.x = clamp((float)(posW.x - posX), 0.0f, 1.0f);
		lpos.y = clamp((float)(posW.y - posY), 0.0f, 1.0f);

		int posXi = (int)posX;
		int posYi = (int)posY;

		unsigned int timeSlice1 = width * width * prevIndex;
		unsigned int timeSlice2 = width * width * nextIndex;
  
		float2 a;
		a.x = 0; 
		a.y = 1;
		float2 b;
		b.x = 3; 
		b.y = 6;
  
		float2 vecMid1 = blend(lpos.y, blend(lpos.x, data[(posXi + 1) + posYi * width + timeSlice1], data[(posXi + 1) + posYi * width + timeSlice1]),
 				 blend(lpos.x, data[posXi + (posYi + 1) * width + timeSlice1], data[(posXi + 1) + (posYi + 1) * width + timeSlice1]));
  
		float2 vecMid2 = blend(lpos.y,  blend(lpos.x, data[posXi + posYi * width + timeSlice2], data[(posXi + 1) + posYi * width + timeSlice2]),
  				 blend(lpos.x, data[posXi + (posYi + 1) * width + timeSlice2], data[(posXi + 1) + (posYi + 1) * width + timeSlice2]));
  

		interpolatedPrev = vecMid1;
		interpolatedNext = vecMid2;

		float localTime = (currentTime - timesteps[prevIndex]) / (timesteps[nextIndex] - timesteps[prevIndex]);

		float2 interpolated = blend(localTime, interpolatedPrev, interpolatedNext);
	  
		pos.x += interpolated.x * timestep;
		pos.y += interpolated.y * timestep;
    }
    output[gid] =  pos;
}
