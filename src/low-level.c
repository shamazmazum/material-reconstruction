#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
#include <fftw3.h>
#include "low-level.h"

#define AN_MAX_DIMENSIONS 3

struct an_update_data {
    cl_uint dimensions[AN_MAX_DIMENSIONS];
    cl_uint point[AN_MAX_DIMENSIONS];
    cl_uint stride[AN_MAX_DIMENSIONS];
};

struct an_gpu_context {
    cl_context       context;
    cl_command_queue queue;
    cl_program       program;

    cl_kernel sparse_ft;
    cl_kernel metric;
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
    cl_mem                 image_saved;
    cl_mem                 image_input;
    cl_mem                 image_output;
    cl_mem                 dist_tmp;
    cl_uint                dimensions[AN_MAX_DIMENSIONS];
    unsigned int           ndims;
    enum an_image_type     type;
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
int an_rfft (const cl_float *array,
             cl_float       *real,
             cl_float       *imag,
             const cl_uint  *dimensions,
             unsigned int    ndims) {
    // Input and output arrays
    cl_float      *in;
    fftwf_complex *out;

    // FFT plan
    fftwf_plan p;

    // Dimensions
    size_t i;
    struct an_array_sizes asizes = an_get_array_sizes (dimensions, ndims);

    // Success
    int ok = 1;

    in  = fftwf_malloc(sizeof(float)         * asizes.real);
    out = fftwf_malloc(sizeof(fftwf_complex) * asizes.complex);
    p   = fftwf_plan_dft_r2c (ndims, (const int*)dimensions, in, out, FFTW_ESTIMATE);

    if (p == NULL) {
        ok = 0;
        goto cleanup;
    }

    // Copy data to the input array and calculate FFT
    memcpy (in, array, sizeof(float)*asizes.real);
    fftwf_execute(p);

    for (i=0; i < asizes.complex; i++) {
        real[i] = out[i][0];
        imag[i] = out[i][1];
    }

cleanup:
    if (p != NULL) {
        fftwf_destroy_plan (p);
    }

    fftwf_free (in);
    fftwf_free (out);

    return ok;
}

int an_irfft (cl_float       *array,
              const cl_float *real,
              const cl_float *imag,
              const cl_uint  *dimensions,
              unsigned int    ndims) {
    // Input and output arrays
    fftwf_complex *in;
    cl_float      *out;

    // FFT plan
    fftwf_plan p;

    // Dimensions
    size_t i;
    struct an_array_sizes asizes = an_get_array_sizes (dimensions, ndims);

    // Success
    int ok = 1;

    in  = fftwf_malloc(sizeof(fftwf_complex) * asizes.complex);
    out = fftwf_malloc(sizeof(cl_float)      * asizes.real);
    p   = fftwf_plan_dft_c2r (ndims, (const int*)dimensions, in, out, FFTW_ESTIMATE);

    if (p == NULL) {
        ok = 0;
        goto cleanup;
    }

    for (i=0; i < asizes.complex; i++) {
        in[i][0] = real[i];
        in[i][1] = imag[i];
    }

    fftwf_execute(p);
    memcpy (array, out, sizeof(float)*asizes.real);

cleanup:
    if (p != NULL) {
        fftwf_destroy_plan (p);
    }

    fftwf_free (in);
    fftwf_free (out);

    return ok;
}

/* Context handling */
void an_destroy_gpu_context (struct an_gpu_context *ctx) {
    if (ctx->reduce != NULL) {
        clReleaseKernel (ctx->reduce);
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
    assert (image->image_saved != image->image_output);

    if (image->dist_tmp != NULL) {
        clReleaseMemObject (image->dist_tmp);
    }
    if (image->image_saved != NULL) {
        clReleaseMemObject (image->image_saved);
    }
    if (image->image_output != NULL) {
        clReleaseMemObject (image->image_output);
    }

    free (image);
}

static cl_mem
create_image_buffer (struct an_image *image,
                     const cl_float  *real,
                     const cl_float  *imag) {
    size_t i;
    struct an_gpu_context *ctx = image->ctx;
    struct an_array_sizes asizes = an_get_array_sizes (image->dimensions, image->ndims);

    cl_mem buffer = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                    asizes.complex * sizeof(cl_float2), NULL, NULL);
    if (buffer == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer (%lu complex floats)\n",
                 asizes.complex);
        goto bad;
    }

    // Copy to GPU
    cl_float2 *mapped_mem = clEnqueueMapBuffer (ctx->queue, buffer, CL_TRUE,
                                                CL_MAP_WRITE, 0, asizes.complex * sizeof(cl_float2),
                                                0, NULL, NULL, NULL);
    if (mapped_mem  == NULL) {
        fprintf (stderr, "Cannot map GPU memory for writing\n");
        goto bad;
    }

    for (i = 0; i < asizes.complex; i++) {
        mapped_mem[i].x = real[i];
        mapped_mem[i].y = imag[i];
    }

    clEnqueueUnmapMemObject (ctx->queue, buffer, mapped_mem, 0, NULL, NULL);

    return buffer;

bad:
    if (buffer != NULL) {
        clReleaseMemObject (buffer);
    }

    return NULL;
}

struct an_image*
an_create_image (struct an_gpu_context *ctx,
                 const cl_float        *real,
                 const cl_float        *imag,
                 const cl_uint         *dimensions,
                 unsigned int           ndims) {
    if (ndims > AN_MAX_DIMENSIONS) {
        fprintf (stderr, "Number of dimensions is too high: %u", ndims);
        return NULL;
    }

    struct an_image *image = malloc (sizeof (struct an_image));
    memset (image, 0, sizeof (struct an_image));
    memcpy (&image->dimensions[0], dimensions, sizeof (cl_uint) * ndims);
    image->ctx   = ctx;
    image->ndims = ndims;
    image->type  = AN_IMAGE_TYPE_IMAGE;

    image->image_input = create_image_buffer (image, real, imag);
    // Create a copy of the image for fast rollback
    image->image_output = create_image_buffer (image, real, imag);
    // Save a pointer to unmodified image
    image->image_saved = image->image_input;

    return image;

bad:
    an_destroy_image (image);
    return NULL;
}

struct an_image*
an_create_corrfn (struct an_gpu_context *ctx,
                  const cl_float        *corrfn,
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

    // We will compare S₂ in this field with the same field of the reconstructed image
    image->image_output = clCreateBuffer (ctx->context, CL_MEM_READ_ONLY,
                                          asizes.complex * sizeof(cl_float), NULL, NULL);
    if (image->image_output == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer (%lu floats)\n",
                 asizes.complex);
        goto bad;
    }

    image->dist_tmp = clCreateBuffer (ctx->context, CL_MEM_READ_WRITE,
                                      asizes.complex * sizeof(cl_float), NULL, NULL);
    if (image->dist_tmp == NULL) {
        fprintf (stderr, "Cannot allocate GPU buffer (%lu floats)\n",
                 asizes.complex);
        goto bad;
    }

    // Copy to GPU
    if (clEnqueueWriteBuffer (ctx->queue, image->image_output, CL_TRUE,
                              0, asizes.complex * sizeof(cl_float),
                              corrfn, 0, NULL, NULL) != CL_SUCCESS) {
        fprintf (stderr, "Cannot copy data to GPU\n");
        goto bad;
    }

    return image;

bad:
    an_destroy_image (image);
    return NULL;
}

void
an_image_store_state (struct an_image *image) {
    assert (image->image_saved != image->image_output);

    cl_mem tmp = image->image_saved;
    image->image_saved  = image->image_output;
    image->image_output = tmp;
    image->image_input  = image->image_saved;
}

void
an_image_rollback (struct an_image *image) {
    assert (image->image_saved != image->image_output);

    cl_mem tmp = image->image_output;
    image->image_input  = image->image_saved;
    image->image_output = image->image_saved;
    image->image_saved  = tmp;
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

    assert (image->image_saved != image->image_output);

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
    cl_float d = delta;
    clSetKernelArg (ctx->sparse_ft, 0, sizeof(cl_mem),                &image->image_input);
    clSetKernelArg (ctx->sparse_ft, 1, sizeof(cl_mem),                &image->image_output);
    clSetKernelArg (ctx->sparse_ft, 2, sizeof(struct an_update_data), &upd);
    clSetKernelArg (ctx->sparse_ft, 3, sizeof(cl_float),              &d);

    clEnqueueNDRangeKernel (ctx->queue, ctx->sparse_ft,
                            image->ndims, NULL, dim, NULL, 0, NULL, NULL);
    image->image_input = image->image_output;
}

int
an_image_get (struct an_image *image,
              cl_float        *real,
              cl_float        *imag) {
    if (image->type != AN_IMAGE_TYPE_IMAGE) {
        return 0;
    }

    size_t i;
    struct an_gpu_context *ctx = image->ctx;
    struct an_array_sizes asizes = an_get_array_sizes (image->dimensions, image->ndims);
    cl_float2 *buffer = clEnqueueMapBuffer (ctx->queue, image->image_output, CL_TRUE,
                                            CL_MAP_READ, 0, asizes.complex * sizeof(cl_float2),
                                            0, NULL, NULL, NULL);
    if (buffer == NULL) {
        fprintf (stderr, "Cannot map image buffer for reading\n");
        return 0;
    }

    for (i = 0; i < asizes.complex; i++) {
        real[i] = buffer[i].x;
        imag[i] = buffer[i].y;
    }

    clEnqueueUnmapMemObject (ctx->queue, image->image_output, buffer, 0, NULL, NULL);
    return 1;
}

int
an_distance (struct an_image *target,
             struct an_image *recon,
             cl_float        *distance) {
    if (target->ctx   != recon->ctx                   ||
        target->ndims != recon->ndims                 ||
        memcmp (target->dimensions,
                recon->dimensions,
                sizeof(cl_uint) * target->ndims) != 0 ||
        target->type  != AN_IMAGE_TYPE_CORRFN         ||
        recon->type   != AN_IMAGE_TYPE_IMAGE) {
        return 0;
    }

    assert(recon->image_output != recon->image_saved);

    struct an_gpu_context *ctx = target->ctx;
    struct an_array_sizes asizes = an_get_array_sizes (target->dimensions, target->ndims);

    size_t gs   = ctx->group_size;
    size_t gssq = gs * gs;

    cl_ulong dim1 = asizes.complex;
    cl_ulong dim2 = gs;

    clSetKernelArg (ctx->metric, 0, sizeof(cl_mem), &target->image_output);
    clSetKernelArg (ctx->metric, 1, sizeof(cl_mem), &recon->image_output);
    clSetKernelArg (ctx->metric, 2, sizeof(cl_mem), &target->dist_tmp);
    clEnqueueNDRangeKernel (ctx->queue, ctx->metric,
                            1, NULL, &dim1, NULL, 0, NULL, NULL);

    clSetKernelArg (ctx->reduce, 0, sizeof(cl_mem), &target->dist_tmp);
    clSetKernelArg (ctx->reduce, 1, sizeof(cl_float) * gs, NULL);
    clSetKernelArg (ctx->reduce, 2, sizeof(cl_ulong), &dim1);
    clEnqueueNDRangeKernel (ctx->queue, ctx->reduce, 1, NULL,
                            &gssq, &gs, 0, NULL, NULL);

#if 0
    clSetKernelArg (ctx->reduce, 0, sizeof(cl_mem), &target->dist_tmp);
    clSetKernelArg (ctx->reduce, 1, sizeof(cl_float) * gs, NULL);
#endif
    clSetKernelArg (ctx->reduce, 2, sizeof(cl_ulong), &dim2);
    clEnqueueNDRangeKernel (ctx->queue, ctx->reduce, 1, NULL,
                            &gs, &gs, 0, NULL, NULL);

    clEnqueueReadBuffer (ctx->queue, target->dist_tmp,
                         CL_TRUE, 0, sizeof (cl_float), distance, 0,
                         NULL, NULL);

    return 1;
}
