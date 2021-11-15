#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <fftw3.h>

#include "anneal-ocl.h"

#define AN_MAX_DIMENSIONS 3

struct an_update_data {
    cl_uint dimensions[AN_MAX_DIMENSIONS];
    cl_uint point[AN_MAX_DIMENSIONS];
    cl_uint stride[AN_MAX_DIMENSIONS];
    cl_uint ndims;
};

struct an_gpu_context {
    cl_context       context;
    cl_command_queue queue;
    cl_program       program;

    cl_kernel sparse_ft;
    cl_kernel metric;
    cl_kernel metric_asym;
    cl_kernel reduce;

    size_t group_size;
};

enum an_image_type {
    AN_IMAGE_TYPE_UNKNOWN,
    AN_IMAGE_TYPE_IMAGE,
    AN_IMAGE_TYPE_CORRFN,
};

struct an_image {
    struct an_gpu_context *ctx;
    cl_mem                 gpu_image;
    cl_uint                dimensions[AN_MAX_DIMENSIONS];
    unsigned int           ndims;
    enum an_image_type     type;
};

struct an_proximeter {
    struct an_gpu_context *ctx;
    struct an_image       *image1;
    struct an_image       *image2;
    cl_mem                 tmp;
};

struct an_array_sizes {
    size_t real;
    size_t complex;
};

static struct an_array_sizes
an_get_array_sizes (const cl_uint *dimensions,
                    unsigned int   ndims) {
    struct an_array_sizes result;
    size_t size = 1;
    unsigned int i;

    for (i=0; i<ndims; i++) {
        size *= dimensions[i];
    }

    result.real = size;
    result.complex = result.real / dimensions[i-1] * (dimensions[i-1]/2 + 1);

    return result;
}

/* Fourier transform */
int an_rfft (const cl_uchar        *array,
             cl_double             *real,
             cl_double             *imag,
             const cl_uint         *dimensions,
             unsigned int           ndims) {
    // Input and output arrays
    cl_double    *in;
    fftw_complex *out;

    // FFT plan
    fftw_plan p;

    // Dimensions
    size_t i;
    struct an_array_sizes asizes = an_get_array_sizes (dimensions, ndims);

    // Success
    int ok = 1;

    in  = fftw_malloc(sizeof(double)       * asizes.real);
    out = fftw_malloc(sizeof(fftw_complex) * asizes.complex);
    p   = fftw_plan_dft_r2c (ndims, (const int*)dimensions, in, out, FFTW_ESTIMATE);

    if (p == NULL) {
        ok = 0;
        goto cleanup;
    }

    // Copy data to the input array and calculate FFT
    for (i=0; i < asizes.real; i++) {
        in[i] = array[i];
    }

    fftw_execute(p);

    for (i=0; i < asizes.complex; i++) {
        real[i] = out[i][0];
        imag[i] = out[i][1];
    }

cleanup:
    if (p != NULL) {
        fftw_destroy_plan (p);
    }

    fftw_free (in);
    fftw_free (out);

    return ok;
}

/* Context handling */
void an_destroy_gpu_context (struct an_gpu_context *ctx) {
    if (ctx->reduce != NULL) {
        clReleaseKernel (ctx->reduce);
    }
    if (ctx->metric_asym != NULL) {
        clReleaseKernel (ctx->metric_asym);
    }
    if (ctx->metric != NULL) {
        clReleaseKernel (ctx->metric);
    }
    if (ctx->sparse_ft != NULL) {
        clReleaseKernel (ctx->sparse_ft);
    }
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

    ctx->sparse_ft = clCreateKernel(ctx->program, "sparse_ft", NULL);
    if (ctx->sparse_ft == NULL) {
        fprintf (stderr, "Cannot create sparse FT kernel\n");
        goto bad;
    }

    ctx->metric = clCreateKernel(ctx->program, "metric", NULL);
    if (ctx->metric == NULL) {
        fprintf (stderr, "Cannot create metric kernel\n");
        goto bad;
    }

    ctx->metric_asym = clCreateKernel(ctx->program, "metric_asym", NULL);
    if (ctx->metric_asym == NULL) {
        fprintf (stderr, "Cannot create asymmetric metric kernel\n");
        goto bad;
    }

    ctx->reduce = clCreateKernel(ctx->program, "reduce", NULL);
    if (ctx->reduce == NULL) {
        fprintf (stderr, "Cannot create reduction kernel\n");
        goto bad;
    }

    return ctx;

bad:
    an_destroy_gpu_context (ctx);
    return NULL;
}

/* Image handling */
void an_destroy_image (struct an_image *image) {
    if (image->gpu_image != NULL) {
        clReleaseMemObject (image->gpu_image);
    }

    free (image);
}

struct an_image*
an_create_image (struct an_gpu_context *ctx,
                 const cl_double       *real,
                 const cl_double       *imag,
                 const cl_uint         *dimensions,
                 unsigned int           ndims) {
    if (ndims > AN_MAX_DIMENSIONS) {
        fprintf (stderr, "Number of dimensions is too high: %u", ndims);
        return NULL;
    }

    struct an_array_sizes asizes = an_get_array_sizes (dimensions, ndims);
    struct an_image *image = malloc (sizeof (struct an_image));

    memset (image, 0, sizeof (struct an_image));
    memcpy (&image->dimensions[0], dimensions, sizeof (cl_uint) * ndims);
    image->ctx   = ctx;
    image->ndims = ndims;
    image->type  = AN_IMAGE_TYPE_IMAGE;

    image->gpu_image = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                       asizes.complex * sizeof(cl_double2), NULL, NULL);
    if (image->gpu_image == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer (%lu complex doubles)\n",
                 asizes.complex);
        goto bad;
    }

    // Copy to GPU
    cl_double2 *image_buf = clEnqueueMapBuffer (image->ctx->queue, image->gpu_image, CL_TRUE,
                                                CL_MAP_WRITE, 0, asizes.complex * sizeof(cl_double2),
                                                0, NULL, NULL, NULL);
    if (image_buf == NULL) {
        goto bad;
    }

    size_t i;
    for (i=0; i < asizes.complex; i++) {
        image_buf[i].x = real[i];
        image_buf[i].y = imag[i];
    }

    clFinish(image->ctx->queue);
    clEnqueueUnmapMemObject (image->ctx->queue, image->gpu_image, image_buf, 0, NULL, NULL);

    return image;

bad:
    an_destroy_image (image);
    return NULL;
}

struct an_image*
an_create_corrfn (struct an_gpu_context *ctx,
                  const cl_double       *corrfn,
                  const cl_uint         *dimensions,
                  unsigned int           ndims) {
    if (ndims > AN_MAX_DIMENSIONS) {
        fprintf (stderr, "Number of dimensions is too high: %u", ndims);
        return NULL;
    }

    struct an_array_sizes asizes = an_get_array_sizes (dimensions, ndims);
    struct an_image *image = malloc (sizeof (struct an_image));

    memset (image, 0, sizeof (struct an_image));
    memcpy (&image->dimensions[0], dimensions, sizeof (cl_uint) * ndims);
    image->ctx   = ctx;
    image->ndims = ndims;
    image->type  = AN_IMAGE_TYPE_CORRFN;

    image->gpu_image = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                       asizes.complex * sizeof(cl_double), NULL, NULL);
    if (image->gpu_image == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer (%lu doubles)\n",
                 asizes.complex);
        goto bad;
    }

    // Copy to GPU
    if (clEnqueueWriteBuffer (ctx->queue, image->gpu_image, CL_TRUE,
                              0, asizes.complex * sizeof(cl_double),
                              corrfn, 0, NULL, NULL) != CL_SUCCESS) {
        fprintf (stderr, "Cannot copy data to GPU\n");
        goto bad;
    }

    return image;

bad:
    an_destroy_image (image);
    return NULL;
}

void an_image_update_fft (struct an_image *image,
                          const cl_uint   *coord,
                          unsigned int     ndims,
                          cl_char          delta) {
    unsigned int i, j;
    struct an_update_data upd;
    size_t dim[AN_MAX_DIMENSIONS];

    if (ndims != image->ndims) {
        fprintf (stderr, "Wrong dimensions\n");
        return;
    }

    if (image->type != AN_IMAGE_TYPE_IMAGE) {
        fprintf (stderr, "Wrong image type\n");
        return;
    }

    upd.ndims = image->ndims;

    for (i=0; i<image->ndims; i++) {
        upd.dimensions[i] = image->dimensions[i];
        dim[i]            = image->dimensions[i];
        upd.point[i]      = coord[i];
        upd.stride[i] = 1;
    }

    dim[i-1] = dim[i-1]/2 + 1;

    for (i=0; i<image->ndims; i++) {
        upd.stride[i] = 1;
        for (j=i+1; j<image->ndims; j++) {
            upd.stride[i] *= dim[j];
        }
    }

    struct an_gpu_context *ctx = image->ctx;
    cl_double d = delta;
    clSetKernelArg (ctx->sparse_ft, 0, sizeof(cl_mem),                &image->gpu_image);
    clSetKernelArg (ctx->sparse_ft, 1, sizeof(struct an_update_data), &upd);
    clSetKernelArg (ctx->sparse_ft, 2, sizeof(cl_double),             &d);

    clEnqueueNDRangeKernel (ctx->queue, ctx->sparse_ft,
                            image->ndims, NULL, dim, NULL, 0, NULL, NULL);
}

// Proximeter (metric calculation)
void an_destroy_proximeter (struct an_proximeter *proximeter) {
    if (proximeter->tmp != NULL) {
        clReleaseMemObject (proximeter->tmp);
    }

    free (proximeter);
}

struct an_proximeter* an_create_proximeter (struct an_image *image1,
                                            struct an_image *image2) {
    if (image1->ctx   != image2->ctx                  ||
        image1->ndims != image2->ndims                ||
        memcmp (image1->dimensions,
                image2->dimensions,
                sizeof(cl_uint) * image1->ndims) != 0 ||
        image2->type  != AN_IMAGE_TYPE_IMAGE) {
        return NULL;
    }

    struct an_array_sizes asizes = an_get_array_sizes (image1->dimensions, image1->ndims);
    struct an_proximeter *proximeter = malloc (sizeof (struct an_proximeter));
    memset (proximeter, 0, sizeof (struct an_proximeter));
    proximeter->image1 = image1;
    proximeter->image2 = image2;
    proximeter->ctx    = image1->ctx;

    proximeter->tmp = clCreateBuffer (proximeter->ctx->context, CL_MEM_READ_WRITE,
                                      asizes.complex * sizeof(cl_double), NULL, NULL);
    if (proximeter->tmp == NULL) {
        fprintf (stderr, "Cannot allocate a temporary buffer (%lu doubles)\n",
                 asizes.complex);
        goto bad;
    }

    return proximeter;

bad:
    an_destroy_proximeter (proximeter);
    return NULL;
}

cl_double an_proximity (struct an_proximeter *proximeter) {
    cl_double metric;

    struct an_image *image1  = proximeter->image1;
    struct an_image *image2  = proximeter->image2;
    struct an_gpu_context *ctx = proximeter->ctx;
    struct an_array_sizes asizes = an_get_array_sizes (image1->dimensions, image1->ndims);
    cl_kernel kmetric = (image1->type == AN_IMAGE_TYPE_IMAGE) ? ctx->metric : ctx->metric_asym;

    size_t gs   = ctx->group_size;
    size_t gssq = gs * gs;

    cl_ulong dim1 = asizes.complex;
    cl_ulong dim2 = gs;

    clSetKernelArg (kmetric, 0, sizeof(cl_mem), &image1->gpu_image);
    clSetKernelArg (kmetric, 1, sizeof(cl_mem), &image2->gpu_image);
    clSetKernelArg (kmetric, 2, sizeof(cl_mem), &proximeter->tmp);
    clEnqueueNDRangeKernel (ctx->queue, kmetric,
                            1, NULL, &dim1, NULL, 0, NULL, NULL);

    clSetKernelArg (ctx->reduce, 0, sizeof(cl_mem), &proximeter->tmp);
    clSetKernelArg (ctx->reduce, 1, sizeof(cl_double) * gs, NULL);
    clSetKernelArg (ctx->reduce, 2, sizeof(cl_ulong), &dim1);
    clEnqueueNDRangeKernel (ctx->queue, ctx->reduce, 1, NULL,
                            &gssq, &gs, 0, NULL, NULL);

#if 0
    clSetKernelArg (ctx->reduce, 0, sizeof(cl_mem), &proximeter->tmp);
    clSetKernelArg (ctx->reduce, 1, sizeof(cl_double) * gs, NULL);
#endif
    clSetKernelArg (ctx->reduce, 2, sizeof(cl_ulong), &dim2);
    clEnqueueNDRangeKernel (ctx->queue, ctx->reduce, 1, NULL,
                            &gs, &gs, 0, NULL, NULL);

    clEnqueueReadBuffer (ctx->queue, proximeter->tmp,
                         CL_TRUE, 0, sizeof (cl_double), &metric, 0,
                         NULL, NULL);

    return metric;
}
