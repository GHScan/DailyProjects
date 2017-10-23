#ifndef CUDA_DEVICE_H
#define CUDA_DEVICE_H



#include <cassert>
#include <string>
#include <exception>
#include <memory>


#include <cuda_runtime.h>



class CUDADevice;



class CUDAException : public std::exception {
public:
    explicit CUDAException(char const *errorString) : mErrorString(errorString) {
    }


    char const* what() const override {
        return mErrorString.c_str();
    }


private:
    std::string mErrorString;
};


#define CUDA_CHECK(err)  \
do { \
    auto _err = err; \
    if (_err != cudaSuccess) \
        throw CUDAException(cudaGetErrorString(_err)); \
 } while(0) 



template<typename T>
struct CUDAArray {
    CUDADevice *Device;
    T *Ptr;
    size_t Length;
};



class CUDADevice {
public:
    CUDADevice(): mDeviceId(gDeviceCount++) {
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
    std::shared_ptr<CUDAArray<T>> Alloc(size_t length) {
        Active();

        void *p;
        CUDA_CHECK(cudaMalloc(&p, length * sizeof(T)));

        return std::shared_ptr<CUDAArray<T>>(new CUDAArray<T>{ this, static_cast<T*>(p), length }, [](auto array)
        {
            array->Device->Active();

            CUDA_CHECK(cudaFree(array->Ptr));
        });
    }


    template<typename T>
    void Copy(T *dest, std::shared_ptr<CUDAArray<T>> src) {
        Active();

        CUDA_CHECK(cudaMemcpy(dest, src->Ptr, src->Length * sizeof(T), cudaMemcpyDeviceToHost));
    }

    template<typename T>
    void Copy(std::shared_ptr<CUDAArray<T>> dest, T const *src) {
        Active();

        CUDA_CHECK(cudaMemcpy(dest->Ptr, src, dest->Length * sizeof(T), cudaMemcpyHostToDevice));
    }

    template<typename T>
    void Copy(std::shared_ptr<CUDAArray<T>> dest, std::shared_ptr<CUDAArray<T>> src) {
        Active();

        CUDA_CHECK(cudaMemcpy(dest->Ptr, src->Ptr, src->Length * sizeof(T), cudaMemcpyDeviceToDevice));
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