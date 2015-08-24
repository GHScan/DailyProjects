#include "stdafx.h"

#include <amp_math.h>

#include "FractialRenderer.h"
#include "ImageGenerater_OpenCL.h"

static int HSV2RGB(TFloat H, TFloat S, TFloat V)
{
    TFloat C = V * S;
    TFloat H1 = H * 6;
    TFloat X = C * (1 - fabs(fmod(H1, 2) - 1));
    TFloat R1, G1, B1;
    switch ((int)H1)
    {
    case 0:
        R1 = C; G1 = X; B1 = 0;
        break;
    case 1:
        R1 = X; G1 = C; B1 = 0;
        break;
    case 2:
        R1 = 0; G1 = C; B1 = X;
        break;
    case 3:
        R1 = 0; G1 = X; B1 = C;
        break;
    case 4:
        R1 = X; G1 = 0; B1 = C;
        break;
    case 5:
        R1 = C; G1 = 0; B1 = X;
        break;
    default:
        R1 = 0; G1 = 0; B1 = 0;
        break;
    }

    TFloat m = V - C;
    int r = (int)((R1 + m) * 255);
    int g = (int)((G1 + m) * 255);
    int b = (int)((B1 + m) * 255);
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

            TFloat value = iteration / maxIteration;
            line[0] = HSV2RGB(value, 1 - value * value, sqrt(value));
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

            TFloat value = iteration / maxIteration;
            line[0] = HSV2RGB(value, 1 - value * value, sqrt(value));
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

static int HSV2RGB_amp(float H, float S, float V) restrict(amp)
{
    using namespace concurrency::fast_math;

    float C = V * S;
    float H1 = H * 6;
    float X = C * (1 - fabs(fmod(H1, 2) - 1));
    float R1, G1, B1;
    switch ((int)H1)
    {
    case 0:
        R1 = C; G1 = X; B1 = 0;
        break;
    case 1:
        R1 = X; G1 = C; B1 = 0;
        break;
    case 2:
        R1 = 0; G1 = C; B1 = X;
        break;
    case 3:
        R1 = 0; G1 = X; B1 = C;
        break;
    case 4:
        R1 = X; G1 = 0; B1 = C;
        break;
    case 5:
        R1 = C; G1 = 0; B1 = X;
        break;
    default:
        R1 = 0; G1 = 0; B1 = 0;
        break;
    }

    float m = V - C;
    int r = (int)((R1 + m) * 255);
    int g = (int)((G1 + m) * 255);
    int b = (int)((B1 + m) * 255);
    return (0 << 24) | (r << 16) | (g << 8) | (b << 0);
}

void OpenCLFractalRenderer::RenderJuliaSet(
    int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
    TFloat minX, TFloat maxX, TFloat minY, TFloat maxY)
{
    mJulia->Run(maxIteration, cx, cy, minX, maxX, minY, maxY);
}

void CppAMPFractalRenderer::RenderMandelbrot(
    int *buffer, int width, int height, int maxIteration,
    TFloat _minX, TFloat _maxX, TFloat _minY, TFloat _maxY) 
{
    using namespace concurrency;
    using namespace concurrency::fast_math;

    float minX = (float)_minX, maxX = (float)_maxX, minY = (float)_minY, maxY = (float)_maxY;

    array_view<int, 2> view = mView;
    parallel_for_each(mView.extent, [=](index<2> idx) restrict(amp)
    {
        int y = idx[0];
        int x = idx[1];
        float fx = minX + (maxX - minX) * x / width;
        float fy = minY + (maxY - minY) * y / height;

        float iteration = 0;
        float zx = 0, zy = 0;
        for (; iteration < maxIteration && zx * zx + zy * zy < 4; ++iteration)
        {
            float newZx = zx * zx - zy * zy + fx;
            zy = 2 * zx * zy + fy;
            zx = newZx;
        }

        if (iteration < maxIteration)
        {
            float logZn = log(zx * zx + zy * zy) / 2;
            float nu = log(logZn / log(2.0f)) / log(2.0f);
            iteration = iteration + 1 - nu;
        }

        float value = iteration / maxIteration;
        view[idx] = HSV2RGB_amp(value, 1 - value * value, sqrt(value));
    });

    view.synchronize();
}

void CppAMPFractalRenderer::RenderJuliaSet(
    int *buffer, int width, int height, int maxIteration, TFloat _cx, TFloat _cy,
    TFloat _minX, TFloat _maxX, TFloat _minY, TFloat _maxY) 
{
    using namespace concurrency;
    using namespace concurrency::fast_math;

    float minX = (float)_minX, maxX = (float)_maxX, minY = (float)_minY, maxY = (float)_maxY;
    float cx = (float)_cx, cy = (float)_cy;

    array_view<int, 2> view = mView;
    parallel_for_each(mView.extent, [=](index<2> idx) restrict(amp)
    {
        int y = idx[0];
        int x = idx[1];
        float fx = minX + (maxX - minX) * x / width;
        float fy = minY + (maxY - minY) * y / height;

        float iteration = 0;
        float zx = fx, zy = fy;
        for (; iteration < maxIteration && zx * zx + zy * zy < 4; ++iteration)
        {
            float newZx = zx * zx - zy * zy + cx;
            zy = 2 * zx * zy + cy;
            zx = newZx;
        }

        if (iteration < maxIteration)
        {
            float logZn = log(zx * zx + zy * zy) / 2;
            float nu = log(logZn / log(2)) / log(2);
            iteration = iteration + 1 - nu;
        }

        float value = iteration / maxIteration;
        view[idx] = HSV2RGB_amp(value, 1 - value * value, sqrt(value));
    });

    view.synchronize();
}