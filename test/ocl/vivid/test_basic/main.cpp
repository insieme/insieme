#include <fstream>
#include "opencv2/opencv.hpp"
#include "vivid.hpp"

int device_use=0;
std::string kernelPath;
std::string exampleImagePath;

bool verify_opencl3d_slice(const int depth, const int width, const int height)
{
	std::cout << "OpenCL DeviceMatrix3D Slice: ";

	const int data_size = depth * width * height;
	float* ref_data = new float[data_size];

	for (int i = 0; i < data_size; i++)
	{
		ref_data[i] = float(std::rand()) / RAND_MAX;
	}

	DeviceMatrixCL3D::Ptr dmpCL = makeDeviceMatrixCL3D(depth, height, width);
	DeviceMatrixCL3D_copyToDevice(*dmpCL, ref_data);

	const int sub_data_size = width * height;

	float* copied_back = new float[sub_data_size];

	for (int slice_ind = 0; slice_ind < depth; slice_ind++)
	{
		DeviceMatrixCL::Ptr slice = makeDeviceMatrixCL(*dmpCL, slice_ind);

		DeviceMatrixCL_copyFromDevice(*slice, copied_back);

		for (int i = 0; i < sub_data_size; i++)
		{
			if (copied_back[i] != ref_data[i + sub_data_size * slice_ind]){
				std::cout << "Ref value not equal at slice: " << slice_ind << ", and index: " << i << std::endl;
				assert(false);
			}
		}
	}
	
	std::cout << "OK" << std::endl;
	delete[] copied_back;
	return true;
}

bool verify_opencl3d(const int depth, const int width, const int height)
{
	std::cout << "OpenCL DeviceMatrix3D: ";
	
	const int data_size = depth * width * height;
	float* ref_data = new float[data_size];

	for (int i = 0; i < data_size; i++)
	{
		ref_data[i] = float(std::rand()) / RAND_MAX;
	}

	DeviceMatrixCL3D::Ptr dmpCL = makeDeviceMatrixCL3D(depth, height, width);

	DeviceMatrixCL3D_copyToDevice(*dmpCL, ref_data);

	float* copied_back = new float[data_size];

	DeviceMatrixCL3D_copyFromDevice(*dmpCL, copied_back);

	std::fstream out_txt("basic.out", std::ios_base::out);

	for (int t=0; t < depth; t++){
		for (int i = 0; i < height; i++){
			for (int j = 0; j < width; j++){
				out_txt << copied_back[t * height * width + i * width + j] << ":" << ref_data[t * height * width + i * width + j] << "\t\t";
			}
			out_txt << std::endl;
		}
		out_txt << "--------------------------------" << std::endl ;
	}
	out_txt.close();

	//for (int i = 0; i < data_size; i++)
	//{
	//	if(copied_back[i] != ref_data[i])
	//	{
	//		std::cout << "FAIL" << std::endl;
	//		std::cout << "mismatch at index " << i << ": " << copied_back[i] << ", " << ref_data[i] << std::endl;
	//
	//		delete[] ref_data;
	//		delete[] copied_back;
	//		return false;
	//	}
	//}

	std::cout << "OK" << std::endl;

	delete[] ref_data;
	delete[] copied_back;
	
	return true;
}

/*bool verify_cuda3d(const int depth, const int width, const int height)
{
	std::cout << "CUDA DeviceMatrix3D: ";

	const int data_size = depth * width * height;
	float* ref_data = new float[data_size];

	for (int i = 0; i < data_size; i++)
	{
		ref_data[i] = float(std::rand()) / RAND_MAX;
	}

	DeviceMatrix3D::Ptr dmpCU = makeDeviceMatrix3D(depth, height, width);

	DeviceMatrix3D_copyToDevice(*dmpCU, ref_data);

	float* copied_back = new float[data_size];

	DeviceMatrix3D_copyFromDevice(*dmpCU, copied_back);

	for (int i = 0; i < data_size; i++)
	{
		if(copied_back[i] != ref_data[i])
		{
			std::cout << "FAIL" << std::endl;
			std::cout << "mismatch at index " << i << ": " << copied_back[i] << ", " << ref_data[i] << std::endl;

			delete[] ref_data;
			delete[] copied_back;
			return false;
		}
	}

	std::cout << "OK" << std::endl;

	delete[] ref_data;
	delete[] copied_back;
	
	return true;
}
*/
void verify_opencl(cv::Mat& exampleImage)
{
	//pull the data
	float* f_imData = (float*) exampleImage.data;

	const int height = exampleImage.size().height;
	const int width = exampleImage.size().width;

	//create a device matrix
	DeviceMatrixCL::Ptr dmpCL = makeDeviceMatrixCL(height, width);

	//copy to the DeviceMatrix
	DeviceMatrixCL_copyToDevice(*dmpCL, f_imData);

	//copy back
	float* copiedBack = new float[height * width];
	DeviceMatrixCL_copyFromDevice(*dmpCL, copiedBack);

	std::cout << "OpenCL DeviceMatrix: " ;

	//verify
	for (int i = 0; i < exampleImage.size().area(); i++)
	{
		if (copiedBack[i] != f_imData[i])
		{
			std::cout << "FAIL" << std::endl;
			std::cout << "mismatch at index " << i << ": " << copiedBack[i] << ", " << f_imData[i] << std::endl;

			delete[] copiedBack;
		}
	}

	std::cout << "OK" << std::endl;

	delete[] copiedBack;
}
/*
void verify_cuda(cv::Mat& exampleImage)
{
	std::cout << "CUDA DeviceMatrix: " ;

	//pull the data
	float* f_imData = (float*) exampleImage.data;

	const int height = exampleImage.size().height;
	const int width = exampleImage.size().width;

	//create a device matrix
	DeviceMatrix::Ptr dmpCU = makeDeviceMatrix(height, width);

	//copy to the DeviceMatrix
	DeviceMatrix_copyToDevice(*dmpCU, f_imData);

	//copy back
	float* copiedBack = new float[height * width];
	DeviceMatrix_copyFromDevice(*dmpCU, copiedBack);

	//verify
	for (int i = 0; i < exampleImage.size().area(); i++)
	{
		if(copiedBack[i] != f_imData[i])
		{
			std::cout << "FAIL" << std::endl;
			std::cout << "mismatch at index " << i << ": " << copiedBack[i] << ", " << f_imData[i] << std::endl;

			delete[] copiedBack;
		}
	}

	std::cout << "OK" << std::endl;

	delete[] copiedBack;
}*/

#define NOCUDA
//#define NOCL
int main(int argc, char* argv[])
{
	if(argc!=2){
		printf("Please provide current path as argument!");
		exit(1);
	}
	kernelPath=string(argv[1])+"/../kernels/";
	exampleImagePath=string(argv[1])+"/../media/kewell1.jpg";


	for (int i = 1; i < argc; i++)
	{
		//if (strcmp(argv[i], "--nocuda") == 0) nocuda = true;
		//if (strcmp(argv[i], "--nocl") == 0) nocl = true;
	}

	cv::Mat exampleImage = cv::imread(exampleImagePath, 0);
	//convert to float
	exampleImage.convertTo(exampleImage, CV_32FC1);
#ifndef NOCL
		verify_opencl(exampleImage);
		std::cout<<"1"<<std::endl;
		verify_opencl3d(2, 600, 416);
		std::cout<<"2"<<std::endl;
		verify_opencl3d_slice(50, 500, 400);
		std::cout<<"3"<<std::endl;
#endif

#ifndef NOCUDA
		verify_cuda(exampleImage);	
		verify_cuda3d(100,20,30);

#endif

	return 0;
}
