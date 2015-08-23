#ifndef FRACTAL_RENDERER_H
#define FRACTAL_RENDERER_H

#include <memory>

typedef double TFloat;

struct IFractalRenderer
{
    virtual ~IFractalRenderer() {}
    virtual void ResetBuffer(int *buffer, int width, int height) = 0;
    virtual void RenderMandelbrot(
        int *buffer, int width, int height, int maxIteration,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) = 0;
    virtual void RenderJuliaSet(
        int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) = 0;
};

class OpenMPFractalRenderer: public IFractalRenderer
{
public:
    virtual void ResetBuffer(int *buffer, int width, int height) {}
    virtual void RenderMandelbrot(
        int *buffer, int width, int height, int maxIteration,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override ;
    virtual void RenderJuliaSet(
        int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override;
};

class OpenCLFractalRenderer : public IFractalRenderer
{
public:
    OpenCLFractalRenderer(int *buffer, int width, int height);
    virtual void ResetBuffer(int *buffer, int width, int height);
    virtual void RenderMandelbrot(
        int *buffer, int width, int height, int maxIteration,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override;
    virtual void RenderJuliaSet(
        int *buffer, int width, int height, int maxIteration, TFloat cx, TFloat cy,
        TFloat minX, TFloat maxX, TFloat minY, TFloat maxY) override;
private:
    std::unique_ptr<class ImageGenerater_OpenCL> mMandelbrot;
    std::unique_ptr<class ImageGenerater_OpenCL> mJulia;
};

#endif