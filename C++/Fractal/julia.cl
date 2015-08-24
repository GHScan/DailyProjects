
#pragma OPENCL EXTENSION cl_khr_fp64 : enable

typedef double TFloat;

static int HSV2RGB(TFloat H, TFloat S, TFloat V)
{
    TFloat C = V * S;
    TFloat H1 = H * 6;
    TFloat X = C * (1 - fabs(fmod(H1, 2) - 1));
    TFloat R1, G1, B1;
    switch ((int)floor(H1))
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
    TFloat iteration = 0;
    for (; iteration < maxIteration && zx * zx + zy * zy < 4; iteration += 1)
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

    TFloat value = iteration / maxIteration;
    buffer[idx] = HSV2RGB(value, 1 - value * value, sqrt(value));
}