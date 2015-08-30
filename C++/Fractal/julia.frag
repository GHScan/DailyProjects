
#if USE_DOUBLE
#define TFloat double
#define TVec4 dvec4
#else
#define TFloat float
#define TVec4 vec4
#endif

vec4 HSV2RGB(TFloat H, TFloat S, TFloat V)
{
    TFloat C = V * S;
    TFloat H1 = H * 6.0;
    TFloat X = C * (1.0 - abs(mod(H1, 2.0) - 1.0));
    TFloat R1, G1, B1;
    switch (int(H1))
    {
    case 0:
        R1 = C; G1 = X; B1 = 0.0;
        break;
    case 1:
        R1 = X; G1 = C; B1 = 0.0;
        break;
    case 2:
        R1 = 0.0; G1 = C; B1 = X;
        break;
    case 3:
        R1 = 0.0; G1 = X; B1 = C;
        break;
    case 4:
        R1 = X; G1 = 0.0; B1 = C;
        break;
    case 5:
        R1 = C; G1 = 0.0; B1 = X;
        break;
    default:
        R1 = 0.0; G1 = 0.0; B1 = 0.0;
        break;
    }

    TFloat m = V - C;
	return vec4(R1 + m, G1 + m, B1 + m, 1.0);
}

uniform TVec4 gRange;
uniform TVec4 gWindowSize;
uniform TFloat gMaxIteration;

void main()
{
	TFloat minX = gRange[0], maxX = gRange[1], minY = gRange[2], maxY = gRange[3];
	TFloat cx = gWindowSize.z, cy = gWindowSize.w;

    TFloat fx = minX + (maxX - minX) * gl_FragCoord.x / gWindowSize.x;
    TFloat fy = minY + (maxY - minY) * gl_FragCoord.y / gWindowSize.y;

    TFloat zx = fx;
    TFloat zy = fy;
    TFloat iteration = 0.0;
    for (; iteration < gMaxIteration && zx * zx + zy * zy < 4.0; iteration += 1.0)
    {
        TFloat newZx = zx * zx - zy * zy + cx;
        zy = 2.0 * zx * zy + cy;
        zx = newZx;
    }

    if (iteration < gMaxIteration)
    {
        TFloat logZn = log(float(zx * zx + zy * zy)) / 2.0;
        TFloat nu = log(float(logZn / log(2.0))) / log(2.0);
        iteration = iteration + 1.0 - nu;
    }

    TFloat value = iteration / gMaxIteration;

	gl_FragColor = HSV2RGB(value, 1.0 - value * value, sqrt(value));
}
