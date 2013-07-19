
#include "pch.h"

#include <time.h>
#include <stdarg.h>

#include <dlfcn.h>

#include <FreeImagePlus.h>
#include <AsmJit/AsmJit.h>

class Timer {
public:
    Timer(const string& name): m_name(name), m_start(clock()) {}
    ~Timer() {
        printf("%s %f sec\n", m_name.c_str(), float(clock() - m_start) / CLOCKS_PER_SEC);
    }
private:
    string m_name;
    clock_t m_start;
};

string format(const char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    static vector<char> buf(256);
    while ((int)buf.size() == vsnprintf(&buf[0], buf.size(), fmt, args)) {
        buf.resize(buf.size() * 3 / 2);
    }
    va_end(args);
    return &buf[0];
}

class JITCompiler {
public:
private:
};

void applyFilterMatrix(
        BYTE *destBuf, const BYTE *srcBuf, int scanW, int imageW, int imageH,
        int *matrix, int matrixW, int matrixH) {
    int fac = 0;
    for (int i = 0; i < matrixW * matrixH; ++i) fac += matrix[i];
    if (fac == 0) fac = 1;

    for (int y = 0; y < imageH; ++y) {
        for (int x = 0; x < imageW; ++x) {

            int sum[3] = {0};

            int *curMatrix = matrix;
            for (int fy = 0; fy < matrixH; ++fy) {
                for (int fx = 0; fx < matrixW; ++fx) {
                    int imgX = x + fx - matrixW / 2, imgY = y + fy - matrixH / 2;
                    imgX = max(min(imgX, imageW), 0);
                    imgY = max(min(imgY, imageH), 0);

                    const BYTE *src = srcBuf + imgY * scanW + imgX * 3;
                    sum[0] += src[0] * *curMatrix;
                    sum[1] += src[1] * *curMatrix;
                    sum[2] += src[2] * *curMatrix;
                    ++curMatrix;
                }
            }

            BYTE *dest = destBuf + y * scanW + x * 3;
            dest[0] = max(0, min(255, sum[0] / fac));
            dest[1] = max(0, min(255, sum[1] / fac));
            dest[2] = max(0, min(255, sum[2] / fac));
        }
    }
}

#define GCC_DLL_FILE_NAME "__test"
class JITCompiler_GCC {
public:
    JITCompiler_GCC(int imageW, int imageH, int scanW, int *matrix, int matrixW, int matrixH) {
        {
            ofstream fo(GCC_DLL_FILE_NAME ".cpp");
            fo << combineCppSrc(imageW, imageH, scanW, matrix, matrixW, matrixH);
        }
        string gccCMD = "g++ -shared -fPIC " GCC_DLL_FILE_NAME ".cpp -O3 -o " GCC_DLL_FILE_NAME ".so";
        ::system(gccCMD.c_str());
        ::system("rm " GCC_DLL_FILE_NAME ".cpp");
        m_dll = ::dlopen(GCC_DLL_FILE_NAME ".so", RTLD_NOW);
        m_fapply = (void(*)(BYTE*, const BYTE*))::dlsym(m_dll, "apply");
    }
    ~JITCompiler_GCC() {
        m_fapply = NULL;
        ::dlclose(m_dll);
        ::system("rm " GCC_DLL_FILE_NAME ".so");
    }
    void apply(BYTE *destBuf, const BYTE *srcBuf) {
        m_fapply(destBuf, srcBuf);
    }
private:
    string combineCppSrc(int imageW, int imageH, int scanW, int *matrix, int matrixW, int matrixH) {
        int fac = 0;
        for (int i = 0; i < matrixW * matrixH; ++i) fac += matrix[i];
        if (fac == 0) fac = 1;

        string r;
        r +=        "#include <algorithm>\n";
        r +=        "using namespace std;\n";
        r +=        "typedef unsigned char BYTE;\n";
        r +=        "extern \"C\" void apply(BYTE *destBuf, const BYTE* srcBuf) {\n";
        r += format("   for (int y = 0; y < %d; ++y){\n", imageH);
        r += format("       for (int x = 0; x < %d; ++x) {\n", imageW);
        r +=        "           int sum[3] = {0};\n";
        int *curMatrix = matrix;
        for (int fy = 0; fy < matrixH; ++fy) {
            for (int fx = 0; fx < matrixW; ++fx, ++curMatrix) {
                r += "{\n";
                r += format("   int imgX = x + %d, imgY = y + %d;\n", fx - matrixW / 2, fy - matrixH / 2);
                r += format("   imgX = max(0, min(%d, imgX));\n", imageW);
                r += format("   imgY = max(0, min(%d, imgY));\n", imageH);
                r += format("   const BYTE *src = srcBuf + imgY * %d + imgX * 3;\n", scanW);
                r += format("   sum[0] += src[0] * %d;\n", *curMatrix);
                r += format("   sum[1] += src[1] * %d;\n", *curMatrix);
                r += format("   sum[2] += src[2] * %d;\n", *curMatrix);
                r += "}\n";
            }
        }
        r += format("           BYTE *dest = destBuf + y * %d + x * 3;\n", scanW);
        r += format("           dest[0] = max(0, min(255, sum[0] / %d));\n", fac);
        r += format("           dest[1] = max(0, min(255, sum[1] / %d));\n", fac);
        r += format("           dest[2] = max(0, min(255, sum[2] / %d));\n", fac);
        r +=        "      }\n";
        r +=        "  }\n";
        r +=        "}";
        return r;
    }
private:
    void *m_dll;
    void (*m_fapply)(BYTE*, const BYTE*);
};

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "usage : %s filter-matrix-file image-file [-ASM] [-GCC] [-n3]\n", argv[0]);
        fprintf(stderr, "\nexample: \n"
             "3 3\n"
             "0 1 0\n"
             "1 1 1\n"
             "0 1 0\n");
        return 1;
    }

    bool isASM = false, isGCC = false;
    int loop = 1;
    for (int i = 3; i < argc; ++i) {
        if (argv[i] == string("-ASM")) isASM = true;
        else if (argv[i] == string("-GCC")) isGCC = true;
        else {
            if (argv[i][0] == '-' && argv[i][1] == 'n') {
                loop = atoi(argv[i] + 2);
            }
        }
    }

    int w, h;
    vector<int> filterMatrix;
    {
        ifstream matrixFile(argv[1]);
        if (!matrixFile) {
            fprintf(stderr, "open filter-matrix-file failed!");
            return 1;
        }

        matrixFile >> w >> h;
        if (w % 2 == 0 || h % 2 == 0) {
            fprintf(stderr, "the width and height of filter-matrix should be odd!");
            return 1;
        }
        filterMatrix.resize(w * h);
        for (int i = 0; i < (int)filterMatrix.size(); ++i) matrixFile >> filterMatrix[i];
    }

    fipImage srcImg;
    if (!srcImg.load(argv[2])) {
        fprintf(stderr, "open image-file failed !");
        return 1;
    }
    if (srcImg.isTransparent()) {
        fprintf(stderr, "warning : image is transparent !");
    }
    srcImg.convertTo24Bits();

    fipImage destImg(FIT_BITMAP, srcImg.getWidth(), srcImg.getHeight(), 24);

    if (isASM) {
    } else if (isGCC) { 
        Timer _timer("total time");
        JITCompiler_GCC jit(srcImg.getWidth(), srcImg.getHeight(), srcImg.getScanWidth(), &filterMatrix[0], w, h);
        {
            Timer _timer(format("apply %d times", loop));
            for (int i = 0; i < loop; ++i) {
                jit.apply(destImg.getScanLine(0), srcImg.getScanLine(0));
            }
        }
    } else {
        Timer _timer("total time");
        {
            Timer _timer(format("apply %d times", loop));
            for (int i = 0; i < loop; ++i) {
                applyFilterMatrix(
                        destImg.getScanLine(0), srcImg.getScanLine(0), srcImg.getScanWidth(), srcImg.getWidth(), srcImg.getHeight(),
                        &filterMatrix[0], w, h);
            }
        }
    }

    destImg.save(format("o_%s", argv[2]).c_str());
}
