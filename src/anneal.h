#pragma once
#define CL_TARGET_OPENCL_VERSION 110
#include <CL/cl.h>

struct an_gpu_context;
struct an_image;

/* Unnormalized S₂ computation */
int an_s2 (const cl_uchar *array,
           float          *s2,
           const cl_uint  *dimensions,
           cl_uint         ndims);

/* GPU context */
struct an_gpu_context*
an_create_gpu_context (const char *program);

void
an_destroy_gpu_context (struct an_gpu_context *ctx);

/* Image object */
struct an_image*
an_create_image (struct an_gpu_context *ctx,
                 const cl_uchar        *image,
                 const cl_ulong        *s2,
                 const cl_uint         *dimensions,
                 unsigned int           ndims);

void
an_destroy_image (struct an_image *image);

int
an_image_flip_pixel (struct an_image *image,
                     const cl_uint   *coord);

int
an_image_get_s2 (struct an_image *image,
                 cl_ulong        *s2);

int
an_image_get_image (struct an_image *image,
                    cl_uchar        *buffer);

/* Distance measurement */
int
an_distance (struct an_image *target,
             struct an_image *recon,
             cl_ulong        *distance);
