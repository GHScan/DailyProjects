
#ifndef IMAGEGENERATER_OPENCL_H
#define IMAGEGENERATER_OPENCL_H

#include <CL\opencl.h>

class ImageGenerater_OpenCL
{
private:
    template<typename ...TArgs>
    struct FillArray;
    template<>
    struct FillArray<>
    {
        static void Invoke(const void *ptrArray[], size_t sizeArray[]) {}
    };
    template<typename T, typename ...TArgs>
    struct FillArray<T, TArgs...>
    {
        static void Invoke(const void *ptrArray[], size_t sizeArray[], T *arg, TArgs* ...args)
        {
            ptrArray[0] = arg;
            sizeArray[0] = sizeof(*arg);
            FillArray<TArgs...>::Invoke(ptrArray + 1 , sizeArray + 1, args...);
        }
    };
    
public:
    ImageGenerater_OpenCL(char const *sourceFile, char const *kernelName, int *buffer, int width, int height);
    ~ImageGenerater_OpenCL();

    template<typename ...TArgs>
    void Run(TArgs ...args)
    {
        const void* ptrArray[sizeof ...(args)];
        size_t sizeArray[sizeof ...(args)];
        FillArray<TArgs...>::Invoke(ptrArray, sizeArray, &args...);
        RunImpl(ptrArray, sizeArray, sizeof...(args));
    }

private:
    void RunImpl(const void* args[], size_t sizes[], int count);

private:
    cl_platform_id mPlatform;
    cl_device_id mDevice;
    cl_context mContext;
    cl_program mProgram;
    cl_command_queue mQueue;
    cl_kernel mKernel;
    cl_mem mMem;
    int *mBuffer;
    int mWidth, mHeight;
};

#endif