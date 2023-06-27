#include <fftw3.h>
#include <math.h>
#include <string.h>
#include <assert.h>
#include "anneal.h"

#define AN_MAX_DIMENSIONS 3

struct an_update_data {
    cl_ulong flat_s2_origin;
    cl_uint dimensions[AN_MAX_DIMENSIONS]; // (or the input image)
    cl_uint im_stride[AN_MAX_DIMENSIONS];
    cl_uint s2_stride[AN_MAX_DIMENSIONS];
    cl_uint s2_shift[AN_MAX_DIMENSIONS];
    cl_uint index[AN_MAX_DIMENSIONS]; // index (into image) of the updated point
};

struct an_gpu_context {
    cl_context       context;
    cl_command_queue queue;
    cl_program       program;

    size_t group_size;
};

struct an_image {
    struct an_gpu_context *ctx;
    struct an_update_data  update_data;

    cl_kernel update_s2;
    cl_kernel update_image;
    cl_kernel metric;
    cl_kernel reduce;

    cl_mem image;
    cl_mem s2;
    cl_mem tmp;

    size_t s2_update_dims[AN_MAX_DIMENSIONS];
    size_t image_update_dims[AN_MAX_DIMENSIONS];

    unsigned int ndims;
};

struct an_array_sizes {
    size_t image;
    size_t s2;
};

static struct an_array_sizes
an_get_array_sizes (const cl_uint *dimensions,
                    unsigned int   ndims) {
    struct an_array_sizes result;
    result.image = 1;
    result.s2 = 1;

    unsigned int i;

    for (i=0; i<ndims; i++) {
        result.image *= dimensions[i];
    }

    for (i=0; i<ndims-1; i++) {
        result.s2 *= dimensions[i];
    }

    result.s2 *= dimensions[ndims - 1] / 2 + 1;
    return result;
}

int
an_rfft (const float   *in,
         fftwf_complex *out,
         const cl_uint *dimensions,
         cl_uint        ndims) {
    float         *input;
    fftwf_complex *output;
    fftwf_plan plan;
    size_t i;

    struct an_array_sizes asizes = an_get_array_sizes(dimensions, ndims);

    input  = fftwf_malloc(sizeof(float)         * asizes.image);
    output = fftwf_malloc(sizeof(fftwf_complex) * asizes.s2);
    plan   = fftwf_plan_dft_r2c (ndims, (cl_int*)dimensions, input, output, FFTW_ESTIMATE);

    if (plan == NULL) {
        return 0;
    }

    memcpy(input, in, sizeof(float) * asizes.image);
    fftwf_execute (plan);
    memcpy(out, output, sizeof(fftwf_complex) * asizes.s2);

    fftwf_free (input);
    fftwf_free (output);
    fftwf_destroy_plan (plan);

    return 1;
}

int
an_irfft (const fftwf_complex *in,
          float               *out,
          const cl_uint       *dimensions,
          cl_uint              ndims) {
    fftwf_complex *input;
    float         *output;
    fftwf_plan plan;
    size_t i;

    struct an_array_sizes asizes = an_get_array_sizes(dimensions, ndims);

    input  = fftwf_malloc(sizeof(fftwf_complex) * asizes.s2);
    output = fftwf_malloc(sizeof(float)         * asizes.image);
    plan   = fftwf_plan_dft_c2r (ndims, (cl_int*)dimensions, input, output, FFTW_ESTIMATE);

    if (plan == NULL) {
        return 0;
    }

    memcpy(input, in, sizeof(fftwf_complex) * asizes.s2);
    fftwf_execute (plan);
    memcpy(out, output, sizeof(float) * asizes.image);

    fftwf_free (input);
    fftwf_free (output);
    fftwf_destroy_plan (plan);

    return 1;
}

/* Context handling */
void an_destroy_gpu_context (struct an_gpu_context *ctx) {
    if (ctx->program != NULL) {
        clReleaseProgram (ctx->program);
    }
    if (ctx->queue != NULL) {
        clReleaseCommandQueue(ctx->queue);
    }
    if (ctx->context != NULL) {
        clReleaseContext(ctx->context);
    }

    free (ctx);
}

struct an_gpu_context* an_create_gpu_context (const char *program) {
    cl_context_properties properties[3];
    cl_uint num_of_platforms=0;
    cl_platform_id platform_id;
    cl_device_id device_id;
    cl_uint num_of_devices=0;

    struct an_gpu_context *ctx = malloc (sizeof (struct an_gpu_context));
    memset (ctx, 0, sizeof (struct an_gpu_context));

    // retreives a list of platforms available
    if (clGetPlatformIDs (1, &platform_id, &num_of_platforms)!= CL_SUCCESS) {
        fprintf(stderr, "Unable to get platform_id\n");
        goto bad;
    }

    // try to get a supported GPU device
    if (clGetDeviceIDs (platform_id, CL_DEVICE_TYPE_GPU, 1, &device_id,
                        &num_of_devices) != CL_SUCCESS) {
        fprintf(stderr, "Unable to get device_id\n");
        goto bad;
    }

    // Get optimal group size
    if (clGetDeviceInfo (device_id, CL_DEVICE_MAX_WORK_GROUP_SIZE, sizeof (size_t),
                         &ctx->group_size, NULL) != CL_SUCCESS) {
        fprintf (stderr, "Cannot get the optimal group size\n");
        goto bad;
    }

    // context properties list - must be terminated with 0
    properties[0]= CL_CONTEXT_PLATFORM;
    properties[1]= (cl_context_properties) platform_id;
    properties[2]= 0;

    ctx->context = clCreateContext (properties, 1, &device_id, NULL, NULL, NULL);
    if (ctx->context == NULL) {
        fprintf (stderr, "Cannot create context\n");
        goto bad;
    }

    ctx->queue = clCreateCommandQueue (ctx->context, device_id, 0, NULL);
    if (ctx->queue == NULL) {
        fprintf (stderr, "Cannot create command queue\n");
        goto bad;
    }

    ctx->program = clCreateProgramWithSource (ctx->context, 1, &program, NULL, NULL);
    if (ctx->program == NULL) {
        fprintf (stderr, "Cannot create program\n");
        goto bad;
    }

    if (clBuildProgram (ctx->program, 0, NULL, NULL, NULL, NULL) != CL_SUCCESS) {
        fprintf(stderr, "Error building program\n");
        char buffer[4096];
        size_t length;
        clGetProgramBuildInfo(ctx->program, device_id, CL_PROGRAM_BUILD_LOG,
                              sizeof(buffer), buffer, &length);
        fprintf(stderr, "%s\n", buffer);
        goto bad;
    }

    return ctx;

bad:
    an_destroy_gpu_context (ctx);
    return NULL;
}


void
an_destroy_image (struct an_image *image) {
    if (image->tmp != NULL) {
        clReleaseMemObject (image->tmp);
    }
    if (image->s2 != NULL) {
        clReleaseMemObject (image->s2);
    }
    if (image->image != NULL) {
        clReleaseMemObject (image->image);
    }
    if (image->update_s2 != NULL) {
        clReleaseKernel (image->update_s2);
    }
    if (image->update_image != NULL) {
        clReleaseKernel (image->update_image);
    }
    if (image->reduce != NULL) {
        clReleaseKernel (image->reduce);
    }
    if (image->metric != NULL) {
        clReleaseKernel (image->metric);
    }

    free (image);
}

static int
create_and_fill_image_buffer (struct an_image      *image,
                              const  cl_uchar      *ptr,
                              struct an_array_sizes asizes) {
    struct an_gpu_context *ctx = image->ctx;

    image->tmp = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                 asizes.s2 * sizeof(cl_ulong), NULL, NULL);
    if (image->tmp == NULL) {
        fprintf (stderr, "Cannot allocate temporary GPU buffer (%lu bytes)\n",
                 asizes.s2);
        return 0;
    }

    image->image = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                   asizes.image * sizeof(cl_uchar), NULL, NULL);
    if (image->image == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer for image (%lu bytes)\n",
                 asizes.image);
        return 0;
    }

    if (clEnqueueWriteBuffer (ctx->queue, image->image, CL_TRUE, 0, sizeof(cl_uchar) * asizes.image,
                              ptr, 0, NULL, NULL) != CL_SUCCESS) {
        fprintf (stderr, "Cannot write image to GPU buffer (%lu bytes)\n",
                 asizes.image);
        return 0;
    }

    return 1;
}

static int
create_and_fill_s2_buffer (struct an_image      *image,
                           const  cl_ulong      *ptr,
                           struct an_array_sizes asizes) {
    struct an_gpu_context *ctx = image->ctx;

    image->s2 = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                asizes.s2 * sizeof(cl_ulong), NULL, NULL);
    if (image->s2 == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer for s2 (%lu bytes)\n",
                 asizes.s2);
        return 0;
    }

    if (clEnqueueWriteBuffer (ctx->queue, image->s2, CL_TRUE, 0, sizeof(cl_ulong) * asizes.s2,
                              ptr, 0, NULL, NULL) != CL_SUCCESS) {
        fprintf (stderr, "Cannot write s2 to GPU buffer (%lu bytes)\n",
                 asizes.s2);
        return 0;
    }

    return 1;
}

struct an_image*
an_create_image (struct an_gpu_context *ctx,
                 const cl_uchar        *buffer,
                 const cl_ulong        *s2,
                 const cl_uint         *dimensions,
                 unsigned int           ndims) {
    assert (s2 != NULL);

    if (ndims > AN_MAX_DIMENSIONS) {
        fprintf (stderr, "Number of dimensions is too high: %u", ndims);
        return NULL;
    }

    struct an_array_sizes asizes = an_get_array_sizes (dimensions, ndims);
    struct an_image *image = malloc (sizeof (struct an_image));
    struct an_update_data *update = &image->update_data;
    int i;

    memset (image, 0, sizeof (struct an_image));
    memcpy(&update->dimensions[0], dimensions, sizeof(cl_uint)*AN_MAX_DIMENSIONS);
    image->ctx   = ctx;
    image->ndims = ndims;

    for (i = 0; i < ndims-1; i++) {
        image->s2_update_dims[i] = dimensions[i];
    }
    image->s2_update_dims[ndims-1] = dimensions[ndims-1] / 2 + 1;

    for (i = 0; i < ndims; i++) {
        image->image_update_dims[i] = 1;
    }

    update->im_stride[ndims-1] = 1;
    update->s2_stride[ndims-1] = 1;
    for (i = image->ndims-2; i >= 0; i--) {
        update->im_stride[i] = update->dimensions[i + 1]    * update->im_stride[i + 1];
        update->s2_stride[i] = image->s2_update_dims[i + 1] * update->s2_stride[i + 1];
    }

    for (i = 0; i < ndims; i++) {
        update->s2_shift[i] = update->dimensions[i] / 2;
        update->flat_s2_origin += update->s2_shift[i] * update->s2_stride[i];
    }

    if (!create_and_fill_s2_buffer (image, s2, asizes)) {
        goto bad;
    }

    if (buffer != NULL) {
        if (!create_and_fill_image_buffer (image, buffer, asizes)) {
            goto bad;
        }

        image->update_s2 = clCreateKernel(ctx->program, "update_s2", NULL);
        if (image->update_s2 == NULL) {
            fprintf (stderr, "Cannot create S2 kernel\n");
            goto bad;
        }
        clSetKernelArg(image->update_s2, 0, sizeof(cl_mem), &image->image);
        clSetKernelArg(image->update_s2, 1, sizeof(cl_mem), &image->s2);

        image->update_image = clCreateKernel(ctx->program, "update_image", NULL);
        if (image->update_image == NULL) {
            fprintf (stderr, "Cannot create image kernel\n");
            goto bad;
        }
        clSetKernelArg(image->update_image, 0, sizeof(cl_mem), &image->image);

        image->metric = clCreateKernel(ctx->program, "metric", NULL);
        if (image->metric == NULL) {
            fprintf (stderr, "Cannot create metric kernel\n");
            goto bad;
        }
        clSetKernelArg(image->metric, 1, sizeof(cl_mem), &image->s2);
        clSetKernelArg(image->metric, 2, sizeof(cl_mem), &image->tmp);

        image->reduce = clCreateKernel(ctx->program, "reduce", NULL);
        if (image->reduce == NULL) {
            fprintf (stderr, "Cannot create reduction kernel\n");
            goto bad;
        }
        clSetKernelArg (image->reduce, 0, sizeof(cl_mem), &image->tmp);
        clSetKernelArg (image->reduce, 1, sizeof(cl_ulong) * ctx->group_size, NULL);
    }

    return image;

bad:
    an_destroy_image (image);
    return NULL;
}

int
an_image_flip_pixel (struct an_image *image,
                     const cl_uint   *coord) {
    struct an_update_data update;
    struct an_gpu_context *ctx = image->ctx;
    int i;

    if (image->image == NULL) {
        fprintf(stderr, "Wrong type of the image\n");
        return 0;
    }

    memcpy(&update, &image->update_data, sizeof(struct an_update_data));
    memcpy(&update.index, coord, sizeof(cl_uint)*AN_MAX_DIMENSIONS);

    clSetKernelArg(image->update_s2, 2, sizeof(struct an_update_data), &update);
    clEnqueueNDRangeKernel(ctx->queue, image->update_s2, image->ndims, NULL,
                           image->s2_update_dims, NULL, 0, NULL, NULL);

    clSetKernelArg(image->update_image, 1, sizeof(struct an_update_data), &update);
    clEnqueueNDRangeKernel(ctx->queue, image->update_image, image->ndims, NULL,
                           image->image_update_dims, NULL, 0, NULL, NULL);
    clFinish(ctx->queue);

    return 1;
}

int an_image_get_s2 (struct an_image *image,
                     cl_ulong        *s2) {
    struct an_array_sizes asizes = an_get_array_sizes (image->update_data.dimensions, image->ndims);
    struct an_gpu_context *ctx = image->ctx;
    int res = clEnqueueReadBuffer(ctx->queue, image->s2, CL_TRUE, 0, sizeof(cl_ulong) * asizes.s2, s2,
                                  0, NULL, NULL);

    return res == CL_SUCCESS;
}

int an_image_get_image (struct an_image *image,
                        cl_uchar        *buffer) {
    if (image->image == NULL) {
        return 0;
    }

    struct an_array_sizes asizes = an_get_array_sizes (image->update_data.dimensions, image->ndims);
    struct an_gpu_context *ctx = image->ctx;
    int res = clEnqueueReadBuffer(ctx->queue, image->image, CL_TRUE, 0, sizeof(cl_uchar) * asizes.image,
                                  buffer, 0, NULL, NULL);

    return res == CL_SUCCESS;
}

int
an_distance (struct an_image *target,
             struct an_image *recon,
             cl_ulong        *distance) {
    if (recon->tmp     == NULL         ||
        recon->metric  == NULL         ||
        recon->reduce  == NULL         ||
        target->ctx   != recon->ctx    ||
        target->ndims != recon->ndims  ||
        memcmp (target->update_data.dimensions,
                recon->update_data.dimensions,
                sizeof(cl_uint) * target->ndims) != 0) {
        return 0;
    }

    struct an_gpu_context *ctx = recon->ctx;
    struct an_array_sizes asizes = an_get_array_sizes (recon->update_data.dimensions, recon->ndims);

    size_t gs   = ctx->group_size;
    size_t gssq = gs * gs;

    cl_ulong dim1 = asizes.s2;
    cl_ulong dim2 = gs;

    clSetKernelArg(recon->metric, 0, sizeof(cl_mem), &target->s2);
    clEnqueueNDRangeKernel (ctx->queue, recon->metric,
                            1, NULL, &dim1, NULL, 0, NULL, NULL);

    clSetKernelArg (recon->reduce, 2, sizeof(cl_ulong), &dim1);
    clEnqueueNDRangeKernel (ctx->queue, recon->reduce, 1, NULL,
                            &gssq, &gs, 0, NULL, NULL);

    clSetKernelArg (recon->reduce, 2, sizeof(cl_ulong), &dim2);
    clEnqueueNDRangeKernel (ctx->queue, recon->reduce, 1, NULL,
                            &gs, &gs, 0, NULL, NULL);
    clFinish(ctx->queue);

    clEnqueueReadBuffer (ctx->queue, recon->tmp,
                         CL_TRUE, 0, sizeof (cl_ulong), distance, 0,
                         NULL, NULL);

    return 1;
}
