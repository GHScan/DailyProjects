
#include "pch.h"

#include <FreeImagePlus.h>
#include <AsmJit/AsmJit.h>

class JITCompiler {
public:
private:
};

void applyFilterMatrix(
        BYTE *destBuf, const BYTE *srcBuf, int scanW, int imageW, int imageH,
        int *matrix, int matrixW, int matrixH) {
    for (int y = 0; y < imageH; ++y) {
        for (int x = 0; x < imageW; ++x) {

            int sum[3] = {0}, fac = 0;

            int imgY = y - matrixH / 2, imgX = x - matrixW / 2;
            int *curMatrix = matrix;
            const BYTE *srcLine = srcBuf + imgY * scanW + imgX * 3;
            for (int fy = 0; fy < matrixH; ++fy, ++imgY, srcLine += scanW) {
                const BYTE *src = srcLine;
                for (int fx = 0; fx < matrixW; ++fx, ++imgX, ++curMatrix, src += 3) {
                    if (imgY < 0 || imgY >= imageH || imgX < 0 || imgX >= imageW) continue;
                    fac += *curMatrix;
                    sum[0] += src[0] * *curMatrix;
                    sum[1] += src[1] * *curMatrix;
                    sum[2] += src[2] * *curMatrix;
                }
            }

            BYTE *dest = destBuf + y * scanW + x * 3;
            if (fac == 0) fac = 1;
            dest[0] = max(min(sum[0] / fac, 255), 0);
            dest[1] = max(min(sum[1] / fac, 255), 0);
            dest[2] = max(min(sum[2] / fac, 255), 0);
        }
    }
}

int main(int argc, char *argv[]) {
    if (argc < 3) {
        fprintf(stderr, "usage : %s filter-matrix-file image-file [-JIT]\n", argv[0]);
        fprintf(stderr, "\nexample: \n"
             "3 3\n"
             "0 1 0\n"
             "1 1 1\n"
             "0 1 0\n");
        return 1;
    }

    bool isJIT = false;
    if (argc >= 4 && argv[3] == string("-JIT")) isJIT = true;
    (void)isJIT;

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

    applyFilterMatrix(
            destImg.getScanLine(0), srcImg.getScanLine(0), srcImg.getScanWidth(), srcImg.getWidth(), srcImg.getHeight(),
            &filterMatrix[0], w, h);

    char outFileName[128];
    sprintf(outFileName, "o_%s", argv[2]);
    destImg.save(outFileName);
}
