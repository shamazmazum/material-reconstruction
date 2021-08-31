#include <sys/time.h>
#include <stdlib.h>
#include <stdio.h>
#include "anneal_ocl.h"

static double gettime ()
{
    struct timeval tv;
    gettimeofday (&tv, NULL);
    return (double)tv.tv_sec + (0.000001 * (double)tv.tv_usec);
}

int main(int argc, char *argv[]) {
    int i;
    double time;

    if (argc != 2) {
        return 1;
    }

    struct an_gpu_context *ctx = an_create_gpu_context (argv[1]);
    if (ctx == NULL) {
        goto cleanup;
    }

    struct an_image2d *image1 = an_create_image2d(ctx, 300, 300);
    if (image1 == NULL) {
        goto cleanup;
    }

    struct an_image2d *image2 = an_create_image2d(ctx, 300, 300);
    if (image2 == NULL) {
        goto cleanup;
    }

    if (!an_image2d_fft (image1)) {
        goto cleanup;
    }

    time = gettime();
    for (i=0; i<50000; i++) {
        int x = arc4random_uniform (300);
        int y = arc4random_uniform (300);
        an_image2d_set (image1, x, y, 1 - an_image2d_get (image1, x, y));
        an_image2d_set (image2, x, y, 1 - an_image2d_get (image2, x, y));
    }
    time = gettime() - time;
    printf("%f\n", time/50000);

    if (!an_image2d_fft (image2)) {
        goto cleanup;
    }

    struct an_proximeter *proximeter = an_create_proximeter (image1, image2);
    if (proximeter == NULL) {
        goto cleanup;
    }

    printf ("%f\n", an_proximity (proximeter));

    time = gettime();
    float acc = 0;
    for (i=0; i<10000; i++) {
        acc += an_proximity (proximeter);
    }
    time = gettime() - time;
    printf("%f\n", time/10000);

cleanup:
    if (proximeter != NULL) {
        an_destroy_proximeter (proximeter);
    }
    if (image1 != NULL) {
        an_destroy_image2d (image1);
    }
    if (image2 != NULL) {
        an_destroy_image2d (image2);
    }
    if (ctx != NULL) {
        an_destroy_gpu_context (ctx);
    }

    return 0;
}
