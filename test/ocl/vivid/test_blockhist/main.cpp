#include "opencv2/opencv.hpp"
#include "vivid.hpp"

#include <iostream>
#include <fstream>
int device_use = 0;
std::string kernelPath;
int main(int argc, char* argv[])
{
	if(argc!=2){
		printf("Please provide kernel path as argument!");
		exit(1);
	}
	kernelPath=argv[1];

		
	const int height = 640;
	const int width = 480;

	const int MAX_BIN = 100;

	float* weights = new float[height * width];
	float* assignments = new float[height * width];

	for (int i = 0; i < height * width; i++)
	{
		weights[i] = float(std::rand()) / RAND_MAX;
		assignments[i] = float(std::rand() % MAX_BIN);
	}
	
	DeviceMatrixCL::Ptr asCL = makeDeviceMatrixCL(height, width);
	DeviceMatrixCL_copyFromDevice(*asCL, assignments);

	DeviceMatrixCL::Ptr wtCL = makeDeviceMatrixCL(height, width);
	DeviceMatrixCL_copyFromDevice(*wtCL, weights);

	DeviceMatrixCL3D::Ptr block_histograms = cell_histogram_dense_cl(
		asCL,
		wtCL,
		MAX_BIN, 8, 
		0, 0,
		height, width);

	delete[] weights;
	delete[] assignments;
//	cv::Mat exampleImage = cv::imread(exampleImagePath, 0);
//
//	//convert to float
//	exampleImage.convertTo(exampleImage, CV_32FC1);
//
//	//pull the data
//	float* f_imData = (float*) exampleImage.data;
//
//	const int height = exampleImage.size().height;
//	const int width = exampleImage.size().width;
//
//	//create a random filterbank
//	const int num_filters = 100;
//	const int filter_dim = 3;
//
//	float* filter_bank = new float[num_filters * filter_dim * filter_dim];
//
//	for (int i = 0; i < num_filters * filter_dim * filter_dim; i++)
//	{
//		filter_bank[i] = float( std::rand() ) / RAND_MAX;
//	}
//
//	//OPENCL Reference
//	DeviceMatrixCL::Ptr dmpCL = makeDeviceMatrixCL(height, width);
//	DeviceMatrixCL_copyToDevice(*dmpCL, f_imData);
//	set_filter_bank_cl(filter_bank, num_filters * filter_dim * filter_dim);
//	DeviceMatrixCL3D::Ptr retdm = filter_frame_cl_3(dmpCL, num_filters, 1, FF_OPTYPE_COSINE);
//	float* retval = new float[height * width * 2];
//	DeviceMatrixCL3D_copyFromDevice(*retdm, retval);
//
//	std::ofstream test_out_cl("testcl.out", std::ios_base::out);
//	for (int j = 0; j < height; j++)
//	{
//		for (int i = 0; i < width; i++)
//		{
//			test_out_cl << retval[j * width + i] << ", ";
//		}
//
//		test_out_cl << std::endl;
//	}
//
//	test_out_cl << std::endl << std::endl << std::endl;
//
//	for (int j = 0; j < height; j++)
//	{
//		for (int i = 0; i < width; i++)
//		{
//			test_out_cl << retval[height * width + j * width + i] << ", ";
//		}
//		test_out_cl << std::endl;
//	}
//	test_out_cl.close();
//
//	delete[] filter_bank;
//	delete[] retval;
//	//delete[] retvalCU;
//
//	return 0;
}
