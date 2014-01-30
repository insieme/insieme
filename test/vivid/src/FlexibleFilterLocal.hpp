#ifndef _FLEXIBLE_FILTER_LOCAL_HPP_
#define _FLEXIBLE_FILTER_LOCAL_HPP_

int update_filter_bank_internal(float* new_filter, int filter_size);

/*
void dist_filter( const DeviceMatrix* frame,
                  const int dim_t, const int dim_y, const int dim_x,
                  const DeviceMatrix3D* filter_bank,
                  DeviceMatrix3D* output,
                  const int optype);
*/

void dist_filter_noargmin(const DeviceMatrix* frame,
                  const int dim_t, const int dim_y, const int dim_x, const int nchannels,
                  DeviceMatrix3D* output,
                  const int optype);

void dist_filter2_d3(const DeviceMatrix* frame,
                  const int dim_t, const int nchannels,
                  DeviceMatrix3D* output,
                  const int optype);

void dist_filter2_d5(const DeviceMatrix* frame,
                  const int dim_t, const int nchannels,
                  DeviceMatrix3D* output,
                  const int optype);

void dist_filter2_d7(const DeviceMatrix* frame,
                  const int dim_t, const int nchannels,
                  DeviceMatrix3D* output,
                  const int optype);

void hist_all_cells(const DeviceMatrix3D* inds_and_weights,
                    DeviceMatrix3D* output,
                    const int cell_size,
                    const int offset_y,
                    const int offset_x,
                    const int max_bin);

void dist_filter2_d3_cl(const DeviceMatrixCL* frame,
					 const int dim_t, const int nchannels,
					 DeviceMatrixCL3D* output,
					 const int optype);

void dist_filter2_d5_cl(const DeviceMatrixCL* frame,
					 const int dim_t, const int nchannels,
					 DeviceMatrixCL3D* output,
					 const int optype);

void dist_filter2_d7_cl(const DeviceMatrixCL* frame,
					 const int dim_t, const int nchannels,
					 DeviceMatrixCL3D* output,
					 const int optype);

void dist_filter_noargmin_cl(const DeviceMatrixCL* frame,
						  const int dim_t, const int dim_y, const int dim_x, const int nchannels,
						  DeviceMatrixCL3D* output,
						  const int optype);

void hist_all_cells_cl(const DeviceMatrixCL3D* inds_and_weights,
                    DeviceMatrixCL3D* output,
                    const int cell_size,
                    const int offset_y,
                    const int offset_x,
                    const int max_bin);

#endif
