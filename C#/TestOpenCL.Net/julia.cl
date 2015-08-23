
#pragma OPENCL EXTENSION cl_khr_fp64 : enable

static int CalculateColor(double iteration, double maxIteration)
{
    double bright = iteration / maxIteration;
    int r = (int)(sqrt(bright) * 256);
    int g = (int)((1 - bright) * 256);
    int b = (int)(0 * 256);
    return (0 << 24) | (r << 16) | (g << 8) | (b << 0);
}

__kernel void main(__global int *buffer, int maxIteration, double cx, double cy, double minX, double maxX, double minY, double maxY)
{
    int height = get_global_size(0);
    int width = get_global_size(1);

    int y = get_global_id(0);
    int x = get_global_id(1);
    int idx = y * width + x;
    double fx = minX + (maxX - minX) * x / width;
    double fy = minY + (maxY - minY) * y / height;

    double zx = fx;
    double zy = fy;
    int iteration = 0;
    for (; iteration < maxIteration && zx * zx + zy * zy < 4; ++iteration)
    {
        double newZx = zx * zx - zy * zy + cx;
        zy = 2 * zx * zy + cy;
        zx = newZx;
    }

    if (iteration < maxIteration)
    {
        double logZn = log(zx * zx + zy * zy) / 2;
        double nu = log(logZn / log(2.0)) / log(2.0);
        iteration = iteration + 1 - nu;
    }

    buffer[idx] = CalculateColor(iteration, maxIteration);
}
