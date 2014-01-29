#include "opencv2/opencv.hpp"
#include "vivid.hpp"
#include "omp.h"
//#include <windows.h>

/*	which device to use
	CPU 0
	GPU 1
*/
int device_use; 
std::string kernelPath;
std::string exampleImagePath;

class FilterBank
{
public:
	FilterBank(const int ndim, const int nfilters):
	  n_filters(nfilters), n_dim(ndim)
	  {
		  data = new float[n_filters * n_dim * n_dim];
		  for (int i = 0; i < n_filters * n_dim * n_dim; i++)
		  {
			  data[i] = float( std::rand() ) / RAND_MAX;
		  }
	  };

	  //TODO: Read from file sFilterBank(const string file_name){};

	  DeviceMatrixCL3D::Ptr apply_cl(DeviceMatrixCL::Ptr dmpCL)
	  {
		//  double tic0= omp_get_wtime();
		  DeviceMatrixCL3D::Ptr retdm = filter_frame_cl_3(dmpCL, n_filters, 1, FF_OPTYPE_COSINE);
	//	  double tic1= omp_get_wtime();
		//  std::cout << "---filter outside time: " << tic1 - tic0 << std::endl;
		  return retdm;
	  };

	  void set_on_device()
	  {
		  set_filter_bank_cl(data, n_filters * n_dim * n_dim);
	  };

	  ~FilterBank()
	  {
		  if (data!=NULL)
		  {
			  delete[] data;
			  data = NULL;
		  }
	  }

	  float* data;

private:
	int n_dim;
	int n_filters;
};

class Classifier
{
public:
	Classifier(const int window_height, const int window_width, const int cell_size, const int block_size, const int dict_size):
	  _window_height(window_height), _window_width(window_width), _dict_size(dict_size),
		  _cell_size(cell_size), _block_size(block_size)
	  {
		  _n_cells_x = _window_width / cell_size;
		  _n_cells_y = _window_height / cell_size;

		  _n_blocks_x = _n_cells_x - _block_size + 1;
		  _n_blocks_y = _n_cells_y - _block_size + 1;

		  _n_total_coeff = _block_size * _block_size * _n_blocks_x * _n_blocks_y, _dict_size;

		  coefficients = new float[_n_total_coeff];

		  classifierCL = makeDeviceMatrixCL(_n_total_coeff / _dict_size, _dict_size);

		  for (int i = 0; i < _n_total_coeff; i++)
		  {
			  coefficients[i] = float( std::rand() ) / RAND_MAX;
		  }

		  DeviceMatrixCL_copyToDevice(*classifierCL, coefficients);
	  };

	  //TODO: read from file Classifier(const string file_name){};

	  DeviceMatrixCL::Ptr apply(DeviceMatrixCL::Ptr blocks)
	  {
		  return pwdist_cl(classifierCL, blocks);
	  };

	  ~Classifier()
	  {
		  if (coefficients != NULL)
		  {
			  delete[] coefficients;
			  coefficients = NULL;
		  }
	  };

private:
	const int _window_height;
	const int _window_width;
	const int _cell_size;
	const int _block_size;
	const int _dict_size;

	int _n_total_coeff;

	int _n_cells_x, _n_cells_y;
	int _n_blocks_x, _n_blocks_y;

	float* coefficients;

	DeviceMatrixCL::Ptr classifierCL;
};

int main(int argc, char* argv[])
{
	if(argc!=2){
		printf("Please provide current path as argument!");
		exit(1);
	}
	kernelPath=string(argv[1])+"/../kernels/";
	exampleImagePath=string(argv[1])+"/../media/kewell1.jpg";
	device_use = 0;

	//create a random filterbank
	const int num_filters = 256;

	//number of pipeline passes
	const int num_iters = 125;

	const int filter_dim = 3;

	FilterBank fb(filter_dim, num_filters);
	fb.set_on_device();

	Classifier clf(128, 64, 8, 2, num_filters);

	//load the image on device
	cv::Mat exampleImage = cv::imread(exampleImagePath, 0);
	//convert to float
	exampleImage.convertTo(exampleImage, CV_32FC1);

	cv::resize(exampleImage, exampleImage, cv::Size(exampleImage.cols, exampleImage.rows));

	if(device_use==0)
		std::cout << "running on CPU" <<std::endl;
	else
		std::cout << "running on GPU" <<std::endl;
	std::cout << "Image dimensions:" << exampleImage.size().height <<" "<< exampleImage.size().width <<std::endl;
	
	//pull the data
	float* f_imData = (float*) exampleImage.data;
	DeviceMatrixCL::Ptr dmpCL = makeDeviceMatrixCL(exampleImage.size().height, exampleImage.size().width);
	DeviceMatrixCL_copyToDevice(*dmpCL, f_imData);


/*	for(int i=0; i<20; i++)
	{

	DeviceMatrixCL3D::Ptr ff_im = fb.apply_cl(dmpCL);
//	tic1= omp_get_wtime();

	DeviceMatrixCL::Ptr block_histogram = cell_histogram_dense_cl(
		ff_im, num_filters, 8, 0, 0, 
		exampleImage.size().height, exampleImage.size().width);
//	tic2= omp_get_wtime();

	DeviceMatrixCL::Ptr result = clf.apply(block_histogram);
	}
	*/
	double tic0, tic1, tic2, tic3;
	double tim1 = 0.0;
	double tim2 = 0.0;
	double tim3 = 0.0;

	for(int i=0; i<num_iters; i++)
	{
	tic0= omp_get_wtime();
	DeviceMatrixCL3D::Ptr ff_im = fb.apply_cl(dmpCL);
	tic1= omp_get_wtime();
	tim1 += tic1 - tic0;

	DeviceMatrixCL::Ptr block_histogram = cell_histogram_dense_cl(
		ff_im, num_filters, 8, 0, 0, 
		exampleImage.size().height, exampleImage.size().width);
	tic2= omp_get_wtime();
	tim2 += tic2 - tic1;

	DeviceMatrixCL::Ptr result = clf.apply(block_histogram);

	TheContext* tc = new TheContext();

	clFinish(tc->getMyContext()->cqCommandQueue);
	tic3 = omp_get_wtime();	
	tim3 += tic3 - tic2;
	}
	
	std::cout << "full pipeline time: " << tim1 + tim2 + tim3 << std::endl;
	std::cout << "filter pipeline time: " << tim1 << std::endl;
	std::cout << "histogram pipeline time: " << tim2 << std::endl;
	std::cout << "classifier pipeline time: " << tim3 << std::endl;

	return 0;
}

/*
windows timer:
LARGE_INTEGER timerFreq_;
QueryPerformanceFrequency(&timerFreq_);
LARGE_INTEGER  st;
QueryPerformanceCounter(&st);



LARGE_INTEGER  et;
QueryPerformanceCounter(&et);
std::cout << (et.QuadPart - st.QuadPart) * 1000 / timerFreq_.QuadPart << "ms" << "\n";
*/
