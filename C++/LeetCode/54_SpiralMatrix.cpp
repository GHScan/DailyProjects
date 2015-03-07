class Solution {
public:
    vector<int> spiralOrder(vector<vector<int> > &matrix) {
        vector<int> result;
        if (matrix.empty()) return result;
        traverseRect(result, matrix, 0, 0, (int)matrix[0].size(), (int)matrix.size());
        return result;
    }
private:
    void traverseRect(vector<int> &result, vector<vector<int> > &matrix, int initX, int initY, int width, int height) {
        for (int x = 0; x < width; ++x) result.push_back(matrix[initY][initX + x]);
        for (int y = 1; y < height - 1; ++y) result.push_back(matrix[initY + y][initX + width - 1]);
        if (height > 1) {
            for (int x = width - 1; x >= 0; --x) result.push_back(matrix[initY + height - 1][initX + x]);
        }
        if (width > 1) {
            for (int y = height - 2; y > 0; --y) result.push_back(matrix[initY + y][initX]);
        }
        if (width > 2 && height > 2) traverseRect(result, matrix, initX + 1, initY + 1, width - 2, height - 2);
    }
};
