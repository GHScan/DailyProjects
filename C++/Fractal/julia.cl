
#pragma OPENCL EXTENSION cl_khr_fp64 : enable

typedef double TFloat;

static int CalculateColor(TFloat iteration, TFloat maxIteration)
{
    TFloat bright = iteration / maxIteration;
    int r = (int)(sqrt(bright) * 256);
    int g = (int)((1 - bright) * 256);
    int b = (int)(0 * 256);
    return (0 << 24) | (r << 16) | (g << 8) | (b << 0);
}

__kernel void main(__global int *buffer, int maxIteration, TFloat cx, TFloat cy, TFloat minX, TFloat maxX, TFloat minY, TFloat maxY)
{
    int height = get_global_size(0);
    int width = get_global_size(1);

    int y = get_global_id(0);
    int x = get_global_id(1);
    int idx = y * width + x;
    TFloat fx = minX + (maxX - minX) * x / width;
    TFloat fy = minY + (maxY - minY) * y / height;

    TFloat zx = fx;
    TFloat zy = fy;
    int iteration = 0;
    for (; iteration < maxIteration && zx * zx + zy * zy < 4; ++iteration)
    {
        TFloat newZx = zx * zx - zy * zy + cx;
        zy = 2 * zx * zy + cy;
        zx = newZx;
    }

    if (iteration < maxIteration)
    {
        TFloat logZn = log(zx * zx + zy * zy) / 2;
        TFloat nu = log(logZn / log(2.0)) / log(2.0);
        iteration = iteration + 1 - nu;
    }

    buffer[idx] = CalculateColor(iteration, maxIteration);
}