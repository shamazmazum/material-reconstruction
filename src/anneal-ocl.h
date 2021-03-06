#pragma once
#define CL_TARGET_OPENCL_VERSION 110
#include <CL/cl.h>

struct an_gpu_context;
struct an_image;

/* Fourier transform */
int an_rfft (const cl_uchar        *array,
             cl_double             *real,
             cl_double             *imag,
             const cl_uint         *dimensions,
             unsigned int           ndims);

/* GPU context */
struct an_gpu_context*
an_create_gpu_context (const char *program);

void
an_destroy_gpu_context (struct an_gpu_context *ctx);

/* Image object */
struct an_image*
an_create_image (struct an_gpu_context *ctx,
                 const cl_double       *real,
                 const cl_double       *imag,
                 const cl_uint         *dimensions,
                 unsigned int           ndims);

struct an_image*
an_create_corrfn (struct an_gpu_context *ctx,
                  const cl_double       *corrfn,
                  const cl_uint         *dimensions,
                  unsigned int           ndims);

void
an_destroy_image (struct an_image *image);

void
an_image_update_fft (struct an_image *image,
                     const cl_uint   *coord,
                     unsigned int     ndims,
                     cl_char          delta);

/* Distance measurement */
int
an_distance (struct an_image *target,
             struct an_image *recon,
             cl_double       *distance);
