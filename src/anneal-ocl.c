#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <program-map.h>
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
    int8_t                *spatial_domain;
    unsigned int           w, h;
    int                    bound;
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

struct an_gpu_context* an_create_gpu_context (const char *program_path) {
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

    struct pm_program_handler ph;
    if (!pm_map_program (&ph, program_path)) {
        perror (pm_get_error ());
        fprintf (stderr, "Cannot load GPU program\n");
        goto bad;
    }

    ctx->program = clCreateProgramWithSource (ctx->context, 1, (const char **)
                                              &(ph.ph_space), NULL, NULL);
    pm_unmap_program (&ph);
    if (ctx->program == NULL) {
        fprintf (stderr, "Cannot create program\n");
        goto bad;
    }

    if (clBuildProgram (ctx->program, 0, NULL, "-cl-fast-relaxed-math", NULL, NULL) != CL_SUCCESS) {
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
void an_destroy_image2d (struct an_image2d *image) {
    if (image->imag != NULL) {
        clReleaseMemObject (image->imag);
    }

    if (image->real != NULL) {
        clReleaseMemObject (image->real);
    }

    free (image->spatial_domain);
    free (image);
}

struct an_image2d* an_create_image2d (struct an_gpu_context *ctx,
                                      unsigned int           w,
                                      unsigned int           h) {
    struct an_image2d *image = malloc (sizeof (struct an_image2d));
    memset (image, 0, sizeof (struct an_image2d));
    image->ctx = ctx;

    image->spatial_domain = malloc(sizeof(int8_t) * w * h);
    image->w = w;
    image->h = h;
    memset (image->spatial_domain, 0, sizeof(int8_t) * w * h);

    image->real = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE, w*h*sizeof(cl_double), NULL, NULL);
    if (image->real == NULL) {
        fprintf (stderr, "Cannot allocate an image %ux%u\n", w, h);
        goto bad;
    }

    image->imag = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE, w*h*sizeof(cl_double), NULL, NULL);
    if (image->imag == NULL) {
        fprintf (stderr, "Cannot allocate an image %ux%u\n", w, h);
        goto bad;
    }

    return image;

bad:
    an_destroy_image2d (image);
    return NULL;
}

int8_t an_image2d_get (struct an_image2d *image, unsigned int y, unsigned int x) {
    int8_t mul = (((x + y) & 1) == 0)? 1: -1;
    unsigned int idx = image->w*y + x;

    return mul * image->spatial_domain[idx];
}

void an_image2d_set (struct an_image2d *image, unsigned int y, unsigned int x, int8_t val) {
    int8_t mul = (((x + y) & 1) == 0)? 1: -1;
    unsigned int idx = image->w*y + x;
    struct an_gpu_context *ctx = image->ctx;

    int8_t oldval = image->spatial_domain[idx];
    image->spatial_domain[idx] = mul * val;

    if (image->bound) {
        size_t dim[2];

        // Recalculate FT on GPU
        cl_double diff = image->spatial_domain[idx] - oldval;
        clSetKernelArg (ctx->sparse_ft2d, 0, sizeof(cl_mem),   &image->real);
        clSetKernelArg (ctx->sparse_ft2d, 1, sizeof(cl_mem),   &image->imag);
        clSetKernelArg (ctx->sparse_ft2d, 2, sizeof(cl_uint),  &x);
        clSetKernelArg (ctx->sparse_ft2d, 3, sizeof(cl_uint),  &y);
        clSetKernelArg (ctx->sparse_ft2d, 4, sizeof(cl_double), &diff);

        dim[0] = image->h;
        dim[1] = image->w;
        clEnqueueNDRangeKernel (ctx->queue, ctx->sparse_ft2d,
                                2, NULL, dim, NULL, 0, NULL, NULL);
        clFinish(ctx->queue);
    }
}

// Calculate FFT and upload to the GPU
int an_image2d_fft (struct an_image2d *image) {
    // Calculate an FFT
    fftw_complex *in, *out;
    fftw_plan p;
    unsigned int i, n = image->w * image->h;
    int ok = 1;

    in  = fftw_malloc(sizeof(fftw_complex) * n);
    out = fftw_malloc(sizeof(fftw_complex) * n);
    p   = fftw_plan_dft_2d (image->h, image->w, in, out, FFTW_FORWARD, FFTW_ESTIMATE);

    if (p == NULL) {
        ok = 0;
        goto cleanup;
    }

    for (i=0; i<n; i++) {
        in[i][0] = image->spatial_domain[i];
        in[i][1] = 0.0;
    }

    fftw_execute(p);

    // Copy to GPU
    cl_double *real_buf = clEnqueueMapBuffer (image->ctx->queue, image->real, CL_TRUE, CL_MAP_WRITE,
                                             0, sizeof(cl_double)*n, 0, NULL, NULL, NULL);
    cl_double *imag_buf = clEnqueueMapBuffer (image->ctx->queue, image->imag, CL_TRUE, CL_MAP_WRITE,
                                             0, sizeof(cl_double)*n, 0, NULL, NULL, NULL);
    if (real_buf == NULL || imag_buf == NULL) {
        ok = 0;
        goto cleanup;
    }

    for (i=0; i<n; i++) {
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

    if (ok) {
        image->bound = 1;
    }

    return ok;
}

void an_image_get_fft (struct an_image2d *image, double *real, double *imag) {
    struct an_gpu_context *ctx = image->ctx;

    clEnqueueReadBuffer (ctx->queue, image->real, CL_TRUE, 0,
                         image->w * image->h * sizeof(cl_double),
                         real, 0, NULL, NULL);
    clEnqueueReadBuffer (ctx->queue, image->imag, CL_TRUE, 0,
                         image->w * image->h * sizeof(cl_double),
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

    unsigned int n = image1->w * image1->h;
    struct an_proximeter *proximeter = malloc (sizeof (struct an_proximeter));
    memset (proximeter, 0, sizeof (struct an_proximeter));
    proximeter->image1 = image1;
    proximeter->image2 = image2;
    proximeter->ctx    = image1->ctx;

    proximeter->tmp = clCreateBuffer (proximeter->ctx->context, CL_MEM_READ_WRITE,
                                      n * sizeof(cl_double), NULL, NULL);
    if (proximeter->tmp == NULL) {
        fprintf (stderr, "Cannot allocate a temporary buffer 1x%u\n", n);
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

    cl_ulong dim1 = image1->w * image1->h;
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
