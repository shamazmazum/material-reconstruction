#define MAX_DIMENSIONS 3

struct update_data {
    unsigned int dimensions[MAX_DIMENSIONS];
    unsigned int point[MAX_DIMENSIONS];
    unsigned int stride[MAX_DIMENSIONS];
};

__kernel void sparse_ft (__global float2 *image,
                         struct update_data upd,
                         float c) {
    float angle = 0;
    size_t idx = 0;
    unsigned int i, ndims = get_work_dim();

    for (i=0; i<ndims; i++) {
        size_t id = get_global_id(i);
        angle += (float)upd.point[i] * (float)id / (float)upd.dimensions[i];
        idx += upd.stride[i] * id;
    }

    angle = 2 * M_PI * angle;
    image[idx].x += c * cos(angle);
    image[idx].y -= c * sin(angle);
}

__kernel void metric (__global float  *image1,
                      __global float2 *image2,
                      __global float  *output) {
    size_t idx = get_global_id(0);
    float abs_sq = dot(image2[idx], image2[idx]);

    output[idx] = pown(image1[idx] - abs_sq, 2);
}

__kernel void reduce (__global float *array,
                      __local  float *tmp,
                      unsigned long length)
{
    size_t global_size = get_global_size(0);
    size_t global_idx = get_global_id(0);
    size_t local_idx = get_local_id(0);
    size_t local_size = get_local_size(0);
    size_t group_num = get_group_id(0);
    size_t i;
    float acc = 0.0;

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
