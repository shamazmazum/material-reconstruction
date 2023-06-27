#define MAX_DIMENSIONS 3

struct update_data {
    unsigned long flat_s2_origin;
    unsigned int dimensions[MAX_DIMENSIONS]; // (or the input image)
    unsigned int im_stride[MAX_DIMENSIONS];
    unsigned int s2_stride[MAX_DIMENSIONS];
    unsigned int s2_shift[MAX_DIMENSIONS];
    unsigned int index[MAX_DIMENSIONS]; // index (into image) of the updated point
};

size_t get_index(unsigned int *stride,
                 unsigned int *index,
                 unsigned int  ndims) {
    size_t i, lindex = 0;
    for (i = 0; i < ndims; i++) {
        lindex += stride[i] * index[i];
    }

    return lindex;
}

// Dimensionality of the kernel = 1×…×1
__kernel void update_image(__global unsigned char *image,
                           struct update_data update) {
    int ndims = get_work_dim();
    size_t index = get_index(update.im_stride, update.index, ndims);

    image[index] = 1 - image[index];
}

// Dimensionality of the kernel = dimensionality of s2 array
__kernel void update_s2(__global unsigned char *image,
                        __global unsigned long *s2,
                        struct update_data update) {
    int i, ndims = get_work_dim();
    int index[MAX_DIMENSIONS];
    size_t flat_index_s2;
    size_t flat_index_image = get_index(update.im_stride, update.index, ndims);

    // Determine sign and update image
    char val = image[flat_index_image];
    char sign = 1 - 2*val;

    for (i = 0; i < ndims; i++) {
        index[i] = get_global_id(i);
    }
    flat_index_s2 = get_index(update.s2_stride, index, ndims);

    if (flat_index_s2 == update.flat_s2_origin) {
        s2[flat_index_s2] += sign;
    } else {
        char diff = 0;
        // Look forward
        for (i = 0; i < ndims; i++) {
            index[i] = (update.dimensions[i] + update.index[i] + get_global_id(i) - update.s2_shift[i])
                % update.dimensions[i];
        }
        flat_index_image = get_index(update.im_stride, index, ndims);
        diff += image[flat_index_image];

        // Look backward
        for (i = 0; i < ndims; i++) {
            index[i] = (update.dimensions[i] + update.index[i] + update.s2_shift[i] - get_global_id(i))
                % update.dimensions[i];
        }
        flat_index_image = get_index(update.im_stride, index, ndims);
        diff += image[flat_index_image];

        s2[flat_index_s2] += sign * diff;
    }
}

__kernel void metric (__global unsigned long *s21,
                      __global unsigned long *s22,
                      __global unsigned long *output) {
    size_t idx = get_global_id(0);
    output[idx] = abs((long)s21[idx] - (long)s22[idx]);
}

__kernel void reduce (__global unsigned long *array,
                      __local  unsigned long *tmp,
                      unsigned long length)
{
    size_t global_size = get_global_size(0);
    size_t global_idx = get_global_id(0);
    size_t local_idx = get_local_id(0);
    size_t local_size = get_local_size(0);
    size_t group_num = get_group_id(0);
    size_t i;
    long acc = 0;

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
