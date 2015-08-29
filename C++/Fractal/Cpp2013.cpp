
#include "stdafx.h"

#include <algorithm>
#include <iomanip>
#include <memory>

#include "RenderWindow.h"
#include "OpenMPFractalRenderer.h"
#include "OpenCLFractalRenderer.h"
#include "CppAMPFractalRenderer.h"

extern IFractalRenderer* CreateCUDAFractalRenderer(int width, int height);
extern IFractalRenderer* CreateOpenGLFractalRenderer();

class FractalRenderWindow : public RenderWindow
{
private:
    TFloat mJuliaCx = -0.74543;
    TFloat mJuliaCy = 0.11301;
    bool mTuneJuliaParameter = false;
    int mRenderShape = 0;
    std::unique_ptr<IFractalRenderer> mRenderer = make_unique<OpenMPFractalRenderer>();
    MouseButton mPressedMouseButton = MouseButton::None;
    TFloat mMouseX, mMouseY;
    TFloat mMinX = -2;
    TFloat mMaxX = 2;
    TFloat mMinY = -2;
    TFloat mMaxY = 2;
    int mMaxIteration = 32;

public:
    FractalRenderWindow()
        : RenderWindow("Fractal", 800, 600)
    {
    }

    TFloat GetScale() const
    {
        return 4 / (mMaxY - mMinY);
    }

    virtual void Setup() override
    {
    }

    virtual void KeyDown(int key) override
    {
        if (key == 'W')
        {
            ++mMaxIteration;
            cout << "Max iteration:" << mMaxIteration << endl;
        }
        else if (key == 'S')
        {
            mMaxIteration = max(mMaxIteration - 1, 1);
            cout << "Max iteration:" << mMaxIteration << endl;
        }
        else if (key == 'Q')
        {
            cout << "input (cx cy) for juliaSet:" << endl;
            cin >> mJuliaCx >> mJuliaCy;
        }
        else if (key == 'A')
        {
            --mRenderShape;
        }
        else if (key == 'D')
        {
            ++mRenderShape;
        }
        else if (key == 'J')
        {
            mTuneJuliaParameter = true;
        }
        else if (key == '1')
        {
            mRenderer = make_unique<OpenMPFractalRenderer>();
            cout << "Switch to OpenMP renderer" << endl; 
        }
        else if (key == '2')
        {
            mRenderer = make_unique<OpenCLFractalRenderer>(GetWidth(), GetHeight());
            cout << "Switch to OpenCL renderer" << endl;
        }
        else if (key == '3')
        {
            mRenderer = make_unique<CppAMPFractalRenderer>(GetWidth(), GetHeight());
            cout << "Switch to C++AMP renderer" << endl;
        }
        else if (key == '4')
        {
            mRenderer.reset(CreateCUDAFractalRenderer(GetWidth(), GetHeight()));
            cout << "Switch to CUDA renderer" << endl;
        }
        else if (key == '5')
        {
            mRenderer.reset(CreateOpenGLFractalRenderer());
            cout << "Switch to OpenGL renderer" << endl;
        }
    }

    virtual void KeyUp(int k) override
    {
        if (k == 'J')
        {
            mTuneJuliaParameter = false;
        }
    }

    virtual void MouseButtonDown(MouseButton button, float x, float y) override
    {
        mPressedMouseButton = button;
        mMouseX = x;
        mMouseY = y;
    }

    virtual void MouseButtonUp(MouseButton button, float x, float y) override
    {
        mPressedMouseButton = MouseButton::None;
    }

    virtual void MouseMove(float x, float y) override
    {
        TFloat dx = x - mMouseX;
        TFloat dy = y - mMouseY;
        mMouseX = x;
        mMouseY = y;

        if (mTuneJuliaParameter)
        {
            mJuliaCx += dx;
            mJuliaCy += dy;
            cout << "cx:" << mJuliaCx << ", cy" << mJuliaCy << endl;
        }
        else if (mPressedMouseButton == MouseButton::Left)
        {
            TFloat moveScale = 1 / GetScale() * 2;
            mMinX += -dx * moveScale, mMaxX += -dx * moveScale;
            mMinY += -dy * moveScale, mMaxY += -dy * moveScale;
            cout << "minX:" << mMinX<< ", minY:" << mMinY << ", maxX:" << mMaxX << ", maxY:" << mMaxY << endl;
        }
        else
        {
            TFloat midX = (mMaxX + mMinX) / 2, halfX = (mMaxX - mMinX) / 2 * (1 + dy);
            TFloat midY = (mMaxY + mMinY) / 2, halfY = (mMaxY - mMinY) / 2 * (1 + dy);
            mMinX = midX - halfX, mMaxX = midX + halfX;
            mMinY = midY - halfY, mMaxY = midY + halfY;
            cout << "scale:" << std::setw(32) << GetScale() << endl;
        }
    }

    virtual void On1SecondElapse() override
    {
        cout << "Fps:" << std::setw(6) << GetFps() << endl;
    }

    virtual void Cleanup() override
    {
    }

    virtual void Resize(int width, int height) override
    {
        RenderWindow::Resize(width, height);
        mRenderer->ResetBuffer(GetWidth(), GetHeight());
        cout << "Resize window to width:" << GetWidth() << ", height" << GetHeight() << endl;
    }

    virtual bool RenderToBuffer() override
    {
        return mRenderer->RenderToBuffer();
    }

    virtual void Render(int *buffer) override
    {
        int width = GetWidth(), height = GetHeight();
        switch (((mRenderShape % 8) + 8) % 8)
        {
        case 0:
            mRenderer->RenderMandelbrot(buffer, width, height, mMaxIteration, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 1:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, mJuliaCx, mJuliaCy, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 2:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, -0.4, 0.6, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 3:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, 0.285, 0, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 4:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, 0.285, 0.01, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 5:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, -0.8, 0.156, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 6:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, -0.74543, 0.11301, mMinX, mMaxX, mMinY, mMaxY);
            break;
        case 7:
            mRenderer->RenderJuliaSet(buffer, width, height, mMaxIteration, -0.1, 0.651, mMinX, mMaxX, mMinY, mMaxY);
            break;
        default:
            throw std::exception("Invlaid shape");
        }
    }
};

int main(int argc, char *argv[])
{
    FractalRenderWindow window;
    window.Run(argv, argc);
}