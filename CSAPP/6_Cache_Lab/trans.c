/* 
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */ 
#include <stdio.h>
#include "cachelab.h"

int is_transpose(int M, int N, int A[N][M], int B[M][N]);

/* 
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded. 
 */
char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    int iOff, jOff, i, j;
    int temp0, temp1, temp2, temp3, temp4, temp5, temp6, temp7;

    if (M == 64 && N == 64) {
        for (iOff = 0; iOff + 8 <= M; iOff += 8) {
            for (jOff = 0; jOff + 8 <= N; jOff += 8) {
                for (j = jOff; j < jOff + 4; ++j) {
                    temp0 = A[j][iOff];
                    temp1 = A[j][iOff+1];
                    temp2 = A[j][iOff+2];
                    temp3 = A[j][iOff+3];
                    temp4 = A[j][iOff+4];
                    temp5 = A[j][iOff+5];
                    temp6 = A[j][iOff+6];
                    temp7 = A[j][iOff+7];
                    B[iOff+0][j]=temp0;
                    B[iOff+1][j]=temp1;
                    B[iOff+2][j]=temp2;
                    B[iOff+3][j]=temp3;
                    B[iOff+1][j+4]=temp4;
                    B[iOff+2][j+4]=temp5;
                    B[iOff+3][j+4]=temp6;
                    B[iOff+0][j+4]=temp7;
                }
                for (j = jOff + 4; j < jOff + 8; ++j) {
                    temp0 = A[j][iOff];
                    temp1 = A[j][iOff+1];
                    temp2 = A[j][iOff+2];
                    temp3 = A[j][iOff+3];
                    temp4 = A[j][iOff+4];
                    temp5 = A[j][iOff+5];
                    temp6 = A[j][iOff+6];
                    temp7 = A[j][iOff+7];
                    B[iOff+4][j-4]=temp0;
                    B[iOff+5][j-4]=temp1;
                    B[iOff+6][j-4]=temp2;
                    B[iOff+7][j-4]=temp3;
                    B[iOff+4][j]=temp4;
                    B[iOff+5][j]=temp5;
                    B[iOff+6][j]=temp6;
                    B[iOff+7][j]=temp7;
                }
                for (i = 3; i >= 0; --i) {
                    j = (i + 1) & 3;
                    temp0 = B[iOff+4+i][jOff];
                    temp1 = B[iOff+4+i][jOff+1];
                    temp2 = B[iOff+4+i][jOff+2];
                    temp3 = B[iOff+4+i][jOff+3];
                    temp4 = B[iOff+j][jOff+4];
                    temp5 = B[iOff+j][jOff+5];
                    temp6 = B[iOff+j][jOff+6];
                    temp7 = B[iOff+j][jOff+7];
                    B[iOff+4+i][jOff] = temp4;
                    B[iOff+4+i][jOff+1] = temp5;
                    B[iOff+4+i][jOff+2] = temp6;
                    B[iOff+4+i][jOff+3] = temp7;
                    B[iOff+j][jOff+4] = temp0;
                    B[iOff+j][jOff+5] = temp1;
                    B[iOff+j][jOff+6] = temp2;
                    B[iOff+j][jOff+7] = temp3;
                }
                for (i = 0; i < 3; ++i) {
                    j = i + 1;
                    temp0 = B[iOff+i][jOff+4];
                    temp1 = B[iOff+i][jOff+5];
                    temp2 = B[iOff+i][jOff+6];
                    temp3 = B[iOff+i][jOff+7];
                    temp4 = B[iOff+j][jOff+4];
                    temp5 = B[iOff+j][jOff+5];
                    temp6 = B[iOff+j][jOff+6];
                    temp7 = B[iOff+j][jOff+7];
                    B[iOff+j][jOff+4] = temp0;
                    B[iOff+j][jOff+5] = temp1;
                    B[iOff+j][jOff+6] = temp2;
                    B[iOff+j][jOff+7] = temp3;
                    B[iOff+i][jOff+4] = temp4;
                    B[iOff+i][jOff+5] = temp5;
                    B[iOff+i][jOff+6] = temp6;
                    B[iOff+i][jOff+7] = temp7;
                }
            }
        }
    } else {
        iOff = 0;
        for (; iOff + 8 <= M; iOff += 8) {
            jOff = 0;
            for (; jOff + 8 <= N; jOff += 8) {
                for (j = jOff; j < jOff + 8; ++j) {
                    temp0 = A[j][iOff];
                    temp1 = A[j][iOff+1];
                    temp2 = A[j][iOff+2];
                    temp3 = A[j][iOff+3];
                    temp4 = A[j][iOff+4];
                    temp5 = A[j][iOff+5];
                    temp6 = A[j][iOff+6];
                    temp7 = A[j][iOff+7];
                    B[iOff][j] = temp0;
                    B[iOff+1][j] = temp1;
                    B[iOff+2][j] = temp2;
                    B[iOff+3][j] = temp3;
                    B[iOff+4][j] = temp4;
                    B[iOff+5][j] = temp5;
                    B[iOff+6][j] = temp6;
                    B[iOff+7][j] = temp7;
                }
            }
            for (j = jOff; j < N; ++j) {
                for (i = iOff; i < iOff + 8; ++i)
                    B[i][j] = A[j][i];
            }
        }
        for (i = iOff; i < M; ++i) {
            for (j = 0; j < N; ++j)
                B[i][j] = A[j][i];
        }
    }
}

/* 
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started. 
 */ 

/* 
 * trans - A simple baseline transpose function, not optimized for the cache.
 */
char trans_desc[] = "Simple row-wise scan transpose";
void trans(int M, int N, int A[N][M], int B[M][N])
{
    int i, j, tmp;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; j++) {
            tmp = A[i][j];
            B[j][i] = tmp;
        }
    }    

}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc); 

    /* Register any additional transpose functions */
    registerTransFunction(trans, trans_desc); 

}

/* 
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

