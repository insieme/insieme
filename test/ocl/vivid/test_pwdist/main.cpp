#include "opencv2/opencv.hpp"
#include "vivid.hpp"
#include "omp.h"


/*	which device to use
	CPU 0
	GPU 1
*/
int device_use; 
std::string kernelPath;

int main(int argc, char* argv[])
{
	device_use = 0;
	if(argc!=2){
		printf("Please provide current path as argument!");
		exit(1);
	}
	kernelPath=string(argv[1])+"/../kernels/";
	
	const int aheight = 3192;
	const int num_dim = 100;
	const int bheight = 4;
	const int owidth = 300;

	float* random1 = new float[aheight * num_dim];
	float* random2 = new float[bheight * num_dim];
	for(int i=0; i<aheight*num_dim; i++) {
		random1[i] = (rand() % 100) / (float)(100);
	}
	for(int i=0; i<bheight*num_dim; i++) {
		random2[i] = (rand() % 100) / (float)(100);
	}

	//create a device matrix
	DeviceMatrixCL::Ptr dmpCL1 = makeDeviceMatrixCL(aheight, num_dim);
	DeviceMatrixCL::Ptr dmpCL2 = makeDeviceMatrixCL(bheight, num_dim);

	//copy to the DeviceMatrix
	DeviceMatrixCL_copyToDevice(*dmpCL1, random1);
	DeviceMatrixCL_copyToDevice(*dmpCL2, random2);
	DeviceMatrixCL::Ptr dmpCLpwdist = pwdist_cl(dmpCL1, dmpCL2);
	double tic0 = omp_get_wtime();
	for (int i = 0; i < 10; ++i) dmpCLpwdist = pwdist_cl(dmpCL1, dmpCL2);
	double tic1 = omp_get_wtime();

	float* retval = new float[aheight * bheight];
	//copy to the DeviceMatrix
	DeviceMatrixCL_copyFromDevice(*dmpCLpwdist, retval);

//	std::cout << "pwdist time: " << tic1 - tic0 << std::endl;
	float* retval2 = new float[aheight * bheight];	
	double tic = omp_get_wtime();
	for (int i=0; i < 10; ++i)
	{
		for (unsigned int i=0; i<aheight; i++){
		    #pragma omp parallel for num_threads(4)
		    for (unsigned int j=0; j<bheight; j++){
		        float sum = 0.0;

		        for (int k = 0; k < num_dim; k++){
		            float dif = (random1[i * num_dim + k] - random2[j * num_dim + k]);
		            sum+=dif*dif;
		        }
		        retval2[i*bheight+j]=sum;
		    }
		}
	}
    double toc = omp_get_wtime();

//	cout << "CPU time: " << toc - tic << std::endl;

	bool error = false;
	for(int i=0; i<aheight*bheight; i++)
		if(abs(retval[i]-retval2[i])>1e-2) 
		{
			error = true;
			printf("%f %f\n",retval[i], retval2[i]);
		}
	if(error)
		printf("output error!\n");

	//OpenCV Reference
	cv::Mat opencv_test1(aheight, num_dim, CV_32FC1, random1);
	cv::Mat opencv_test2(bheight, num_dim, CV_32FC1, random2);
	cv::Mat opencv_ret_exp;
	cv::Mat opencv_ret(aheight, bheight, CV_32FC1);
	cv::Mat diff;
	cv::Mat sq;
	double tic_cv = omp_get_wtime();
	for (int i = 0; i < 10; ++i)
	{
		for (int ai = 0; ai < aheight; ++ai)
		{
			cv::Mat arow = opencv_test1.row(ai);
			for (int bi = 0; bi < bheight; ++bi)
			{
				cv::Mat brow = opencv_test2.row(bi);
				diff = (arow - brow);
				opencv_ret.data[ai * opencv_ret.step + bi] = cv::norm(diff);
			}
		}
	}
	double toc_cv = omp_get_wtime();

//	std::cout << "pwdist opencv: " << (toc_cv - tic_cv) << std::endl;

	return 0;
}
