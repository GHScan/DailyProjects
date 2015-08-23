#include "stdafx.h"

#include "FractialRenderer.h"
#include "ImageGenerater_OpenCL.h"

static int CalculateColor(TFloat iteration, TFloat maxIteration)
{
    TFloat bright = iteration / maxIteration;
    int r = int(sqrt(bright) * 256);
    int g = int((1 - bright) * 256);
    int b = int(0 * 256);
    return (0 << 24) | (r << 16) | (g << 8) | (b << 0);
}

void OpenMPFractalRenderer::RenderMandelbrot(
    int *buffer, int width, int height, int maxIteration,
    TFloat minX, TFloat maxX, TFloat minY, TFloat maxY)
{
    TFloat fdy = 1.0 / height * (maxY - minY), fdx = 1.0 / width * (maxX - minX);

#pragma omp parallel for 
    for (int y = 0; y < height; ++y)
    {
        TFloat fy = minY + fdy * y;
        int *line = buffer + y * width;

        TFloat fx = minX;
        for (int x = 0; x < width; ++x, fx += fdx, ++line)
        {
            TFloat iteration = 0;
            TFloat zx = 0, zy = 0;
            for (; iteration < maxIteration && zx * zx + zy * zy < 4; ++iteration)
            {
                TFloat newZx = zx * zx - zy * zy + fx;
                zy = 2 * zx * zy + fy;
                zx = newZx;
            }

            if (iteration < maxIteration)
            {
                TFloat logZn = log(zx * zx + zy * zy) / 2;
                TFloat nu = log(logZn / log(2)) / log(2);
                iteration = iteration + 1 - nu;
            }

            line[0] = CalculateColor(iteration, maxIteration);
        }
    }
}

void OpenMPFractalRenderer::RenderJuliaSet(
    int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
    TFloat minX, TFloat maxX, TFloat minY, TFloat maxY)
{
    TFloat fdy = 1.0 / height * (maxY - minY), fdx = 1.0 / width * (maxX - minX);

#pragma omp parallel for 
    for (int y = 0; y < height; ++y)
    {
        TFloat fy = minY + fdy * y;
        int *line = buffer + y * width;

        TFloat fx = minX;
        for (int x = 0; x < width; ++x, fx += fdx, ++line)
        {
            TFloat iteration = 0;
            TFloat zx = fx, zy = fy;
            for (; iteration < maxIteration && zx * zx + zy * zy < 4; ++iteration)
            {
                TFloat newZx = zx * zx - zy * zy + cx;
                zy = 2 * zx * zy + cy;
                zx = newZx;
            }

            if (iteration < maxIteration)
            {
                TFloat logZn = log(zx * zx + zy * zy) / 2;
                TFloat nu = log(logZn / log(2)) / log(2);
                iteration = iteration + 1 - nu;
            }

            line[0] = CalculateColor(iteration, maxIteration);
        }
    }
}

OpenCLFractalRenderer::OpenCLFractalRenderer(int *buffer, int width, int height)
: mMandelbrot(make_unique<ImageGenerater_OpenCL>("mandelbrot.cl", "main", buffer, width, height)),
mJulia(make_unique<ImageGenerater_OpenCL>("julia.cl", "main", buffer, width, height))
{
}

void OpenCLFractalRenderer::ResetBuffer(int *buffer, int width, int height)
{
    mMandelbrot = make_unique<ImageGenerater_OpenCL>("mandelbrot.cl", "main", buffer, width, height);
    mJulia = make_unique<ImageGenerater_OpenCL>("julia.cl", "main", buffer, width, height);
}

void OpenCLFractalRenderer::RenderMandelbrot(
    int *buffer, int width, int height, int maxIteration,
    TFloat minX, TFloat maxX, TFloat minY, TFloat maxY)
{
    mMandelbrot->Run(maxIteration, minX, maxX, minY, maxY);
}

void OpenCLFractalRenderer::RenderJuliaSet(
    int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
    TFloat minX, TFloat maxX, TFloat minY, TFloat maxY)
{
    mJulia->Run(maxIteration, cx, cy, minX, maxX, minY, maxY);
}