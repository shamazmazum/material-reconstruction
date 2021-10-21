#pragma OPENCL EXTENSION cl_khr_fp64 : enable

#define MAX_DIMENSIONS 3

struct update_data {
    unsigned int dimensions[MAX_DIMENSIONS];
    unsigned int point[MAX_DIMENSIONS];
    unsigned int stride[MAX_DIMENSIONS];
    unsigned int ndims;
};

__kernel void sparse_ft (__global double   *real,
                         __global double   *imag,
                         struct update_data upd,
                         double c) {
    double angle = 0;
    size_t idx = 0;
    unsigned int i;

    for (i=0; i<upd.ndims; i++) {
        size_t id = get_global_id(i);
        angle += (double)upd.point[i] * (double)id / (double)upd.dimensions[i];
        idx += upd.stride[i] * id;
    }

    angle = 2 * M_PI * angle;
    real[idx] += c * cos(angle);
    imag[idx] -= c * sin(angle);
}

__kernel void metric (__global double *real1,
                      __global double *imag1,
                      __global double *real2,
                      __global double *imag2,
                      __global double *output) {
    size_t idx = get_global_id(0);

    double abs_sq1 = pown(real1[idx], 2) + pown(imag1[idx], 2);
    double abs_sq2 = pown(real2[idx], 2) + pown(imag2[idx], 2);

    output[idx] = pown(abs_sq1 - abs_sq2, 2);
}

__kernel void reduce (__global double *array,
                      __local  double *tmp,
                      unsigned long length)
{
    size_t global_size = get_global_size(0);
    size_t global_idx = get_global_id(0);
    size_t local_idx = get_local_id(0);
    size_t local_size = get_local_size(0);
    size_t group_num = get_group_id(0);
    size_t i;
    double acc = 0.0;

    for (i=0; i<length; i+=global_size) {
        size_t idx = global_idx + i;
        if (idx < length) acc += array[idx];
    }

    tmp[local_idx] = acc;
    barrier (CLK_LOCAL_MEM_FENCE);

    for (i=local_size>>1; i>0; i>>=1) {
        if (local_idx < i) tmp[local_idx] += tmp[local_idx + i];
        barrier (CLK_LOCAL_MEM_FENCE);
    }

    if (local_idx == 0) {
        array[group_num] = tmp[0];
    }
}
