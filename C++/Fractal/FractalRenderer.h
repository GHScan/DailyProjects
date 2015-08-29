#ifndef FRACTAL_RENDERER_H
#define FRACTAL_RENDERER_H

#define USE_DOUBLE 0

#if USE_DOUBLE
typedef double TFloat;
#else
typedef float TFloat;
#endif

struct IFractalRenderer
{
    virtual ~IFractalRenderer() {}
    virtual bool RenderedToBuffer() const { return true;  }
    virtual void ResetBuffer(int *buffer, int width, int height) = 0;
    virtual void RenderMandelbrot(
        int *buffer, int width, int height, int maxIteration,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) = 0;
    virtual void RenderJuliaSet(
        int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) = 0;
};

#endif