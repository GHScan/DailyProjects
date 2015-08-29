#include "stdafx.h"

#include <sstream>
#include <fstream>
#include <string>

#include "ImageGenerater_OpenCL.h"
#include "FractalRenderer.h"

static inline void CheckCLError(cl_int err, char const * name)
{
    if (err != CL_SUCCESS)
    {
        std::ostringstream so;
        so << "OpenCL ERROR: " << name << " (" << err << ")" << endl;
        throw std::exception(reinterpret_cast<char const *>(so.str().c_str()));
    }
}

std::string ReadFile(char const *fileName)
{
    std::string content;
    std::ifstream fi(fileName);
    for (std::string line; getline(fi, line);)
    {
        content += line;
        content += '\n';
    }
    return content;
}

ImageGenerater_OpenCL::ImageGenerater_OpenCL(char const *sourceFile, char const *kernelName, int *buffer, int width, int height)
    : mBuffer(buffer), mWidth(width), mHeight(height)
{
    int size = width * height * sizeof(*mBuffer);

    cl_int err = clGetPlatformIDs(1, &mPlatform, nullptr);
    CheckCLError(err, "clGetPlatformIDs");

    err = clGetDeviceIDs(mPlatform, CL_DEVICE_TYPE_GPU, 1, &mDevice, nullptr);
    CheckCLError(err, "clGetDeviceIDs");

    mContext = clCreateContext(nullptr, 1, &mDevice, nullptr, nullptr, &err);
    CheckCLError(err, "clCreateContext");

    {
        std::string source = ReadFile(sourceFile);
        char const *p = source.c_str();
        auto length = source.size();
        mProgram = clCreateProgramWithSource(mContext, 1, &p, &length, &err);
        CheckCLError(err, "clCreateProgramWithSource");
    }

#if USE_DOUBLE
    char const *options = "-D USE_DOUBLE=1";
#else
    char const *options = "-D USE_DOUBLE=0";
#endif
    err = clBuildProgram(mProgram, 1, &mDevice, options, nullptr, nullptr);
    if (err != CL_SUCCESS)
    {
        size_t logSize;
        err = clGetProgramBuildInfo(mProgram, mDevice, CL_PROGRAM_BUILD_LOG, 0, nullptr, &logSize);
        CheckCLError(err, "clGetProgramBuildInfo");
        std::string log(logSize, 0);
        err = clGetProgramBuildInfo(mProgram, mDevice, CL_PROGRAM_BUILD_LOG, log.size(), &log[0], &logSize);
        CheckCLError(err, "clGetProgramBuildInfo");
        throw std::exception(("Build failed: " + log).c_str());
    }

    mQueue = clCreateCommandQueue(mContext, mDevice, 0, &err);
    CheckCLError(err, "clCreateCommandQueue");

    mMem = clCreateBuffer(mContext, CL_MEM_WRITE_ONLY, size, nullptr, &err);
    CheckCLError(err, "clCreateBuffer");

    mKernel = clCreateKernel(mProgram, kernelName, &err);
    CheckCLError(err, "clCreateKernel");

    err = clSetKernelArg(mKernel, 0, sizeof(mMem), &mMem);
    CheckCLError(err, "clSetKernelArg");
}

ImageGenerater_OpenCL::~ImageGenerater_OpenCL()
{
    cl_int err = clReleaseKernel(mKernel);
    CheckCLError(err, "clReleaseKernel");

    err = clReleaseMemObject(mMem);
    CheckCLError(err, "clReleaseMemObject");

    err = clReleaseCommandQueue(mQueue);
    CheckCLError(err, "clReleaseCommandQueue");

    err = clReleaseProgram(mProgram);
    CheckCLError(err, "clReleaseProgram");

    err = clReleaseContext(mContext);
    CheckCLError(err, "clReleaseContext");
}

void ImageGenerater_OpenCL::RunImpl(const void* args[], size_t sizes[], int count)
{
    cl_int err;
    for (int i = 0; i < count; ++i)
    {
        err = clSetKernelArg(mKernel, i + 1, sizes[i], args[i]);
        CheckCLError(err, "clSetKernelArg");
    }

    size_t globalSizes[] = { mHeight, mWidth };
    err = clEnqueueNDRangeKernel(mQueue, mKernel, 2, nullptr, globalSizes, nullptr, 0, nullptr, nullptr);
    CheckCLError(err, "clEnqueueNDRangeKernel");

    clEnqueueReadBuffer(mQueue, mMem, CL_TRUE, 0, mWidth * mHeight * sizeof(*mBuffer), mBuffer, 0, nullptr, nullptr);
    CheckCLError(err, "clEnqueueReadBuffer");
}