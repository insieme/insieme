#ifdef INSIEME
#include "ocl_device.h"
#endif
#include "LinearAlgebra.cl"

float2 make_float2(float x, float y)
{
    float2 foo;
    foo.x = x;
    foo.y = y;
    return foo;
}

float2 blend(float factor, float2 value1, float2 value2)
{
	float factor2 = 1.0 - factor;
	
	return make_float2(
		factor2 * value1.x + factor * value2.x,
		factor2 * value1.y + factor * value2.y
		);
}

float2 interpolateSpatial(
    __global float2 *data, // = width*height*numTimesteps
    unsigned int width,
    unsigned int height,
    float2 dataOrigin,
    float2 dataCellSize,
    unsigned int timeIndex,
    float2 pos // position to interpolate at in physical space
    )
{
  float2 posW = make_float2((pos.x - dataOrigin.x) / dataCellSize.x, (pos.y - dataOrigin.y) / dataCellSize.y);

  // posXi,posYi is integral coordinate of "upper left corner"
  float posX = floor(posW.x);
  float posY = floor(posW.y);

  posX = clamp(posX, 0.0f, (float)(width - 2));
  posY = clamp(posY, 0.0f, (float)(height - 2));
  
  // get local coordinates
  float2 lpos = make_float2(clamp((float)(posW.x - posX), 0.0f, 1.0f), clamp((float)(posW.y - posY), 0.0f, 1.0f));

  unsigned int timeSlice = width * height * timeIndex;
  
  int posXi = (int)posX;
  int posYi = (int)posY;
  
  float2 vecUpper = blend(lpos.x, data[posXi + posYi * width + timeSlice], data[(posXi + 1) + posYi * width + timeSlice]);
  float2 vecLower = blend(lpos.x, data[posXi + (posYi + 1) * width + timeSlice], data[(posXi + 1) + (posYi + 1) * width + timeSlice]);
  float2 vecMid = blend(lpos.y, vecUpper, vecLower);

  return vecMid;
}

/*
#pragma insieme mark
__kernel
void computeFlowMap(
    __global float2 *data // = width*height*numTimesteps
    , unsigned int width
    , unsigned int height
    , float2 dataOrigin
    , float2 dataCellSize
    , __global float *timesteps // = numTimesteps
    , unsigned int numTimesteps
    , float startTime
    , float advectionTime // kann negativ sein, dann wird rueckwaerts integriert
    , __global float2 *output // = outputWidth * outputHeight
    , unsigned int outputWidth
    , unsigned int outputHeight
    , float2 outputOrigin
    , float2 outputCellSize
    )
{
    // Position im Compute-Grid
    //unsigned int x = get_global_id(0);// get_group_id(0) * get_local_size(0) + get_local_id(0);
    //unsigned int y = get_global_id(1);// get_group_id(1) * get_local_size(1) + get_local_id(1);

    unsigned int index = get_global_id(0);

	//if(id > (width*height)) return;
	uint x = index % width;
	uint y = index / width;


    // Sind wir noch innerhalb der Compute-Domain?
    if (x >= outputWidth || y >= outputHeight)
    {
        return;
    }

    const unsigned int numSteps = 1000;

    float timestep = advectionTime / numSteps;

    float2 pos = make_float2(outputOrigin.x + x * outputCellSize.x, outputOrigin.y + y * outputCellSize.y);

    for (unsigned int step = 0; step < numSteps; ++step)
    {
        float currentTime = startTime + step * timestep;
	  
		// previous time index
		int prevIndex = -1;

		for (unsigned int previous = 0; previous < numTimesteps; ++previous)
		{
			if (timesteps[previous] <= currentTime)
			{
					prevIndex = previous;
			}
		}
	  
		if (prevIndex < 0 || prevIndex > numTimesteps - 2)
		{
			output[index].x = 0;
			output[index].y = 0;
			return;
	//	    asm("trap;");
	//	    break;
		}

		// next time index
		int nextIndex = prevIndex + 1;
	  
		float2 interpolatedPrev;
		float2 interpolatedNext;

  float2 posW = make_float2((pos.x - dataOrigin.x) / dataCellSize.x, (pos.y - dataOrigin.y) / dataCellSize.y);

  // posXi,posYi is integral coordinate of "upper left corner"
  float posX = floor(posW.x);
  float posY = floor(posW.y);

  posX = clamp(posX, 0.0f, (float)(width - 2));
  posY = clamp(posY, 0.0f, (float)(height - 2));
  
  // get local coordinates
  float2 lpos = make_float2(clamp((float)(posW.x - posX), 0.0f, 1.0f), clamp((float)(posW.y - posY), 0.0f, 1.0f));

  int posXi = (int)posX;
  int posYi = (int)posY;

  unsigned int timeSlice1 = width * height * prevIndex;
  unsigned int timeSlice2 = width * height * nextIndex;
  
  float2 a = make_float2(0, 1);
  float2 b = make_float2(3,6);
  
  float2 vecMid1 = blend(lpos.y, blend(lpos.x, data[(posXi + 1) + posYi * width + timeSlice1], data[(posXi + 1) + posYi * width + timeSlice1]),
  								 blend(lpos.x, data[posXi + (posYi + 1) * width + timeSlice1], data[(posXi + 1) + (posYi + 1) * width + timeSlice1]));
  
  float2 vecMid2 = blend(lpos.y, blend(lpos.x, data[posXi + posYi * width + timeSlice2], data[(posXi + 1) + posYi * width + timeSlice2]),
  								 blend(lpos.x, data[posXi + (posYi + 1) * width + timeSlice2], data[(posXi + 1) + (posYi + 1) * width + timeSlice2]));
  

		interpolatedPrev = vecMid1;//interpolateSpatial(data, width, height, dataOrigin, dataCellSize, prevIndex, pos);
		interpolatedNext = vecMid2;//interpolateSpatial(data, width, height, dataOrigin, dataCellSize, nextIndex, pos);

		float localTime = (currentTime - timesteps[prevIndex]) / (timesteps[nextIndex] - timesteps[prevIndex]);

		float2 interpolated = blend(localTime, interpolatedPrev, interpolatedNext);
	  
		pos.x += interpolated.x * timestep;
		pos.y += interpolated.y * timestep;
    }

    output[index] =  pos;
}
*/

__kernel
void computeFTLE(
    __global float2 * flowMap // = output von computeFlowMap, outputWidth * outputHeight
    , unsigned int outputWidth
    , unsigned int outputHeight
    , float2 dataCellSize
    , float advectionTime // kann negativ sein fuer rueckwaertsintegration
    , __global float * output // = outputWidth * outputHeight
    )
{
    // Position im Compute-Grid
//    unsigned int x = get_global_id(0);//get_group_id(0) * get_local_size(0) + get_local_id(0);
//    unsigned int y = get_global_id(1);//get_group_id(1) * get_local_size(1) + get_local_id(1);
  
    // Linearer Index fuer die Ausgabe
    unsigned int index = get_global_id(0);

	//if(id > (width*height)) return;
	uint x = index % outputWidth;
	uint y = index / outputWidth;


    // Sind wir noch innerhalb der Compute-Domain?
    if (x > 0 && y > 0 && x < outputWidth - 1 && y < outputHeight - 1)
    {
        float2 left   = flowMap[index - 1];
		float2 right  = flowMap[index + 1];
		float2 top    = flowMap[index - outputWidth];
		float2 bottom = flowMap[index + outputWidth];
	
		float2 delta2 = make_float2(2.0f * dataCellSize.x, 2.0f * dataCellSize.y);
	
		fmat2 jacobi;
	
		jacobi[0][0] = (right.x  - left.x) / delta2.x;
		jacobi[0][1] = (bottom.x - top.x)  / delta2.y;
		jacobi[1][0] = (right.y  - left.y) / delta2.x;
		jacobi[1][1] = (bottom.y - top.y)  / delta2.y;
	
		fmat2 jacobiT;
		fmat2 cauchy;
		fmat2 cauchySymm;
	
		fmat2trp(jacobi, jacobiT);
		fmat2mul(jacobiT, jacobi, cauchy);
		fmat2symm(cauchy, cauchySymm);
	
		fvec2 eigenvalues;
	
		fmat2eigenvalues(cauchySymm, eigenvalues);
	
		float maxEigenvalue = fmax(eigenvalues[0], eigenvalues[1]);
	
		output[index] = 1.0 / fabs(advectionTime) * log(sqrt(maxEigenvalue));
    }
}
