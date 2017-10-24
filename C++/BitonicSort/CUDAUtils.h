#ifndef CUDA_DEVICE_H
#define CUDA_DEVICE_H



#include <cassert>
#include <string>
#include <exception>
#include <memory>


#include <cuda_runtime.h>



class CUDADevice;
using CUDADevicePtr = std::shared_ptr<CUDADevice>;



class CUDAException 
    : public std::exception {
public:

    explicit CUDAException(
        char const *errStr, 
        char const *file, int line) {

        char buf[256];
        sprintf_s(buf, "%s(%d): %s", file, line, errStr);
        mMessage = buf;
    }


    char const* what() const override {
        return mMessage.c_str();
    }


private:
    std::string mMessage;
};


#define CUDA_CHECK(err)  \
do { \
    auto _err = err; \
    if (_err != cudaSuccess) \
        throw CUDAException(cudaGetErrorString(_err), __FILE__, __LINE__); \
 } while(0) 



template<typename T>
struct CUDAArray {
    CUDADevice *Device;
    T *Ptr;
    size_t Length;
};

template<typename T>
using CUDAArrayPtr = std::shared_ptr<CUDAArray<T>>;



class CUDADevice {
public:

    CUDADevice()
        : mDeviceId(gDeviceCount++) {
    }


    ~CUDADevice() noexcept(false) {
        if (--gDeviceCount == 0) {
            if (gCurrentDeviceId != -1) {
                CUDA_CHECK(cudaDeviceReset());
                gCurrentDeviceId = -1;
            }
        }
    }


    template<typename T>
    CUDAArrayPtr<T> Alloc(size_t length) {
        Active();

        void *p;
        CUDA_CHECK(cudaMalloc(&p, length * sizeof(T)));

        return CUDAArrayPtr<T>(
            new CUDAArray<T>{ this, static_cast<T*>(p), length }, 
            [](auto array)
        {
            array->Device->Active();

            CUDA_CHECK(cudaFree(array->Ptr));

            delete array;
        });
    }


    template<typename T>
    void Copy(T *dest, CUDAArrayPtr<T> src) {
        Active();

        CUDA_CHECK(
            cudaMemcpy(
                dest, 
                src->Ptr, 
                src->Length * sizeof(T), 
                cudaMemcpyDeviceToHost));
    }

    template<typename T>
    void Copy(CUDAArrayPtr<T> dest, T const *src) {
        Active();

        CUDA_CHECK(
            cudaMemcpy(
                dest->Ptr,
                src,
                dest->Length * sizeof(T),
                cudaMemcpyHostToDevice));
    }

    template<typename T>
    void Copy(CUDAArrayPtr<T> dest, CUDAArrayPtr<T> src) {
        Active();

        CUDA_CHECK(
            cudaMemcpy(
                dest->Ptr, 
                src->Ptr, 
                src->Length * sizeof(T), 
                cudaMemcpyDeviceToDevice));
    }


    void Synchronize() {
        Active();

        CUDA_CHECK(cudaDeviceSynchronize());
    }


    void CheckLastError() {
        Active();

        CUDA_CHECK(cudaGetLastError());
    }


private:

    void Active() {
        if (gCurrentDeviceId != mDeviceId) {
            CUDA_CHECK(cudaSetDevice(mDeviceId));
            gCurrentDeviceId = mDeviceId;
        }
    }


private:
    static size_t gDeviceCount;
    static size_t gCurrentDeviceId;


private:
    size_t mDeviceId;
};


#endif