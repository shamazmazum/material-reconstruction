#pragma once
#define CL_TARGET_OPENCL_VERSION 110
#include <stdint.h>
#include <CL/cl.h>

struct an_gpu_context;
struct an_image2d;
struct an_proximeter;

struct an_gpu_context* an_create_gpu_context (const char *program_path);
void an_destroy_gpu_context (struct an_gpu_context *ctx);

struct an_image2d* an_create_image2d (struct an_gpu_context *ctx, unsigned int w, unsigned int h);
void an_destroy_image2d (struct an_image2d *image);

int8_t an_image2d_get (struct an_image2d *image, unsigned int y, unsigned int x);
void  an_image2d_set (struct an_image2d *image, unsigned int y, unsigned int x, int8_t val);
// Call when image is filled with data
int an_image2d_fft (struct an_image2d *image);
// For debugging
void an_image_get_fft (struct an_image2d *image, double *real, double *imag);

struct an_proximeter* an_create_proximeter (struct an_image2d *image1, struct an_image2d *image2);
void an_destroy_proximeter (struct an_proximeter *proximeter);
cl_double an_proximity (struct an_proximeter *proximeter);
