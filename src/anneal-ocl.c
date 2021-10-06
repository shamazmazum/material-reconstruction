#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <fftw3.h>

#include "anneal-ocl.h"

struct an_gpu_context {
    cl_context       context;
    cl_command_queue queue;
    cl_program       program;

    cl_kernel sparse_ft2d;
    cl_kernel metric;
    cl_kernel reduce;

    size_t group_size;
};

struct an_image2d {
    struct an_gpu_context *ctx;
    cl_mem                 real;
    cl_mem                 imag;
    cl_uint                w, h;
};

struct an_proximeter {
    struct an_gpu_context *ctx;
    struct an_image2d     *image1;
    struct an_image2d     *image2;
    cl_mem                 tmp;
};

/* Context handling */
void an_destroy_gpu_context (struct an_gpu_context *ctx) {
    if (ctx->reduce != NULL) {
        clReleaseKernel (ctx->reduce);
    }
    if (ctx->metric != NULL) {
        clReleaseKernel (ctx->metric);
    }
    if (ctx->sparse_ft2d != NULL) {
        clReleaseKernel (ctx->sparse_ft2d);
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

    ctx->sparse_ft2d = clCreateKernel(ctx->program, "sparse_ft2d", NULL);
    if (ctx->sparse_ft2d == NULL) {
        fprintf (stderr, "Cannot create sparse FT kernel\n");
        goto bad;
    }

    ctx->metric = clCreateKernel(ctx->program, "metric", NULL);
    if (ctx->metric == NULL) {
        fprintf (stderr, "Cannot create metric kernel\n");
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

/* Calculate FFT and upload to the GPU */
static int an_image2d_fft (struct an_image2d *image, const uint8_t *array) {
    // Input and output arrays
    double       *in;
    fftw_complex *out;

    // FFT plan
    fftw_plan p;

    // Dimensions
    unsigned int i;
    unsigned int n_real    = image->h * image->w;
    unsigned int n_complex = image->h * (image->w/2 + 1);

    // Success
    int ok = 1;

    in  = fftw_malloc(sizeof(double)       * n_real);
    out = fftw_malloc(sizeof(fftw_complex) * n_complex);
    p   = fftw_plan_dft_r2c_2d (image->h, image->w, in, out, FFTW_ESTIMATE);

    if (p == NULL) {
        ok = 0;
        goto cleanup;
    }

    // Copy data to the input array and calculate FFT
    for (i=0; i < n_real; i++) {
        in[i] = array[i];
    }

    fftw_execute(p);

    // Copy to GPU
    cl_double *real_buf = clEnqueueMapBuffer (image->ctx->queue, image->real, CL_TRUE, CL_MAP_WRITE,
                                             0, sizeof(cl_double) * n_complex, 0, NULL, NULL, NULL);
    cl_double *imag_buf = clEnqueueMapBuffer (image->ctx->queue, image->imag, CL_TRUE, CL_MAP_WRITE,
                                             0, sizeof(cl_double) * n_complex, 0, NULL, NULL, NULL);
    if (real_buf == NULL || imag_buf == NULL) {
        ok = 0;
        goto cleanup;
    }

    for (i=0; i < n_complex; i++) {
        real_buf[i] = out[i][0];
        imag_buf[i] = out[i][1];
    }

cleanup:
    if (real_buf != NULL) {
        clEnqueueUnmapMemObject (image->ctx->queue, image->real, real_buf, 0, NULL, NULL);
    }

    if (imag_buf != NULL) {
        clEnqueueUnmapMemObject (image->ctx->queue, image->imag, imag_buf, 0, NULL, NULL);
    }

    if (p != NULL) {
        fftw_destroy_plan (p);
    }

    fftw_free (in);
    fftw_free (out);
    clFinish(image->ctx->queue);

    return ok;
}

void an_destroy_image2d (struct an_image2d *image) {
    if (image->imag != NULL) {
        clReleaseMemObject (image->imag);
    }

    if (image->real != NULL) {
        clReleaseMemObject (image->real);
    }

    free (image);
}

struct an_image2d*
an_create_image2d (struct an_gpu_context *ctx,
                   const uint8_t         *array,
                   unsigned int           w,
                   unsigned int           h) {
    unsigned int n_complex = h * (w/2 + 1);
    struct an_image2d *image = malloc (sizeof (struct an_image2d));

    memset (image, 0, sizeof (struct an_image2d));
    image->ctx = ctx;
    image->w = w;
    image->h = h;

    image->real = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                  n_complex * sizeof(cl_double), NULL, NULL);
    if (image->real == NULL) {
        fprintf (stderr, "Cannot allocate an image %ux%u\n", w, h);
        goto bad;
    }

    image->imag = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                  n_complex * sizeof(cl_double), NULL, NULL);
    if (image->imag == NULL) {
        fprintf (stderr, "Cannot allocate an image %ux%u\n", w, h);
        goto bad;
    }

    if (!an_image2d_fft (image, array)) {
        fprintf (stderr, "Failed to perform FFT\n");
        goto bad;
    }

    return image;

bad:
    an_destroy_image2d (image);
    return NULL;
}

void an_image2d_update_fft (struct an_image2d *image,
                            unsigned int       y,
                            unsigned int       x,
                            int8_t             delta) {
    struct an_gpu_context *ctx = image->ctx;
    size_t dim[2];

    // Recalculate FT on GPU
    cl_double d = delta;
    clSetKernelArg (ctx->sparse_ft2d, 0, sizeof(cl_mem),    &image->real);
    clSetKernelArg (ctx->sparse_ft2d, 1, sizeof(cl_mem),    &image->imag);
    clSetKernelArg (ctx->sparse_ft2d, 2, sizeof(cl_uint),   &x);
    clSetKernelArg (ctx->sparse_ft2d, 3, sizeof(cl_uint),   &y);
    clSetKernelArg (ctx->sparse_ft2d, 4, sizeof(cl_uint),   &image->w);
    clSetKernelArg (ctx->sparse_ft2d, 5, sizeof(cl_uint),   &image->h);
    clSetKernelArg (ctx->sparse_ft2d, 6, sizeof(cl_double), &d);

    dim[0] = image->h;
    dim[1] = image->w/2 + 1;
    clEnqueueNDRangeKernel (ctx->queue, ctx->sparse_ft2d,
                            2, NULL, dim, NULL, 0, NULL, NULL);
    clFinish(ctx->queue);
}

void an_image_get_fft (struct an_image2d *image, double *real, double *imag) {
    struct an_gpu_context *ctx = image->ctx;
    unsigned int n_complex = image->h * (image->w/2 + 1);

    clEnqueueReadBuffer (ctx->queue, image->real, CL_TRUE, 0,
                         n_complex * sizeof(cl_double),
                         real, 0, NULL, NULL);
    clEnqueueReadBuffer (ctx->queue, image->imag, CL_TRUE, 0,
                         n_complex * sizeof(cl_double),
                         imag, 0, NULL, NULL);
}

// Proximeter (metric calculation)
void an_destroy_proximeter (struct an_proximeter *proximeter) {
    if (proximeter->tmp != NULL) {
        clReleaseMemObject (proximeter->tmp);
    }

    free (proximeter);
}

struct an_proximeter* an_create_proximeter (struct an_image2d *image1,
                                            struct an_image2d *image2) {
    if (image1->ctx != image2->ctx ||
        image1->w   != image2->w   ||
        image1->h   != image2->h) {
        return NULL;
    }

    unsigned int n_complex = image1->h * (image1->w/2 + 1);
    struct an_proximeter *proximeter = malloc (sizeof (struct an_proximeter));
    memset (proximeter, 0, sizeof (struct an_proximeter));
    proximeter->image1 = image1;
    proximeter->image2 = image2;
    proximeter->ctx    = image1->ctx;

    proximeter->tmp = clCreateBuffer (proximeter->ctx->context, CL_MEM_READ_WRITE,
                                      n_complex * sizeof(cl_double), NULL, NULL);
    if (proximeter->tmp == NULL) {
        fprintf (stderr, "Cannot allocate a temporary buffer 1x%u\n", n_complex);
        goto bad;
    }

    return proximeter;

bad:
    an_destroy_proximeter (proximeter);
    return NULL;
}

cl_double an_proximity (struct an_proximeter *proximeter) {
    cl_double metric;

    struct an_image2d *image1  = proximeter->image1;
    struct an_image2d *image2  = proximeter->image2;
    struct an_gpu_context *ctx = proximeter->ctx;

    size_t gs   = ctx->group_size;
    size_t gssq = gs * gs;

    cl_ulong dim1 = image1->h * (image1->w/2 + 1);
    cl_ulong dim2 = gs;

    clSetKernelArg (ctx->metric, 0, sizeof(cl_mem), &image1->real);
    clSetKernelArg (ctx->metric, 1, sizeof(cl_mem), &image1->imag);
    clSetKernelArg (ctx->metric, 2, sizeof(cl_mem), &image2->real);
    clSetKernelArg (ctx->metric, 3, sizeof(cl_mem), &image2->imag);
    clSetKernelArg (ctx->metric, 4, sizeof(cl_mem), &proximeter->tmp);

    clEnqueueNDRangeKernel (ctx->queue, ctx->metric,
                            1, NULL, &dim1, NULL, 0, NULL, NULL);
    clFinish(ctx->queue);

    clSetKernelArg (ctx->reduce, 0, sizeof(cl_mem), &proximeter->tmp);
    clSetKernelArg (ctx->reduce, 1, sizeof(cl_double) * gs, NULL);
    clSetKernelArg (ctx->reduce, 2, sizeof(cl_ulong), &dim1);
    clEnqueueNDRangeKernel (ctx->queue, ctx->reduce, 1, NULL,
                            &gssq, &gs, 0, NULL, NULL);
    clFinish(ctx->queue);

#if 0
    clSetKernelArg (ctx->reduce, 0, sizeof(cl_mem), &proximeter->tmp);
    clSetKernelArg (ctx->reduce, 1, sizeof(cl_double) * gs, NULL);
#endif
    clSetKernelArg (ctx->reduce, 2, sizeof(cl_ulong), &dim2);
    clEnqueueNDRangeKernel (ctx->queue, ctx->reduce, 1, NULL,
                            &gs, &gs, 0, NULL, NULL);
    clFinish(ctx->queue);

    clEnqueueReadBuffer (ctx->queue, proximeter->tmp,
                         CL_TRUE, 0, sizeof (cl_double), &metric, 0,
                         NULL, NULL);

    return metric;
}
