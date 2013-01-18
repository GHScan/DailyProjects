#include "pch.h" 

#include <assert.h>

#include <vector>
#include <algorithm>
#include <string>

void split(std::vector<std::string>& tokens, 
        const std::string& src, const std::string& seps, bool compress)
{
    char sepFlags[256] = {0};
    for (size_t i = 0; i < seps.size(); ++i) sepFlags[seps[i]] = 1;

    std::string s;
    for (size_t i = 0; i < src.size(); ++i) {
        if (sepFlags[src[i]]) {
            if (s.size() > 0 || !compress) tokens.push_back(s);
            s.clear();
        }
        else s.push_back(src[i]);
    }
    if (s.size() > 0 || !compress) tokens.push_back(s);
}

int main()
{
    {
        std::string refs[] = {"", "", ""};
        std::vector<std::string> tokens;
        split(tokens, ",,", ",", false);
        assert(std::equal(tokens.begin(), tokens.end(), refs));
    }
    {
        std::string refs[] = {"a", "", "", "b", "c"};
        std::vector<std::string> tokens;
        split(tokens, "a...b,c", ".#,", false);
        assert(std::equal(tokens.begin(), tokens.end(), refs));
    }
    {
        std::string refs[] = {"a", "b", "aa"};
        std::vector<std::string> tokens;
        split(tokens, "#!!a!@b@#aa", "#@!", true);
        assert(std::equal(tokens.begin(), tokens.end(), refs));
    }
}
