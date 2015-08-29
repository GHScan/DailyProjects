#ifndef OPENMP_FRACTAL_RENDERER_H
#define OPENMP_FRACTAL_RENDERER_H

#include "FractalRenderer.h"

class OpenMPFractalRenderer : public IFractalRenderer
{
public:
    virtual void ResetBuffer(int width, int height)
    {
    }

    virtual void RenderMandelbrot(
        int *buffer, int width, int height, int maxIteration,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override
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

    virtual void RenderJuliaSet(
        int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override
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

private:
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
};

#endif