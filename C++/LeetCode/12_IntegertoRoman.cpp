class Solution {
public:
	string intToRoman(int num) {
		char romanChar[][2] = {{'I', 'V', }, {'X', 'L', }, {'C', 'D', }, {'M', '_'}};
		string result;
		for (int i = 0; num > 0; num /= 10, ++i) {
			int digit = num % 10;
			switch (digit % 5) {
			case 0: case 1: case 2: case 3:
				for (int j = 0; j < digit % 5; ++j) result.push_back(romanChar[i][0]);
				if (digit >= 5) result.push_back(romanChar[i][1]);
				break;
			case 4:
				if (digit > 5) result.push_back(romanChar[i + 1][0]);
				else result.push_back(romanChar[i][1]);
				result.push_back(romanChar[i][0]);
				break;
			}
		}
		return string(result.rbegin(), result.rend());
	}
};
