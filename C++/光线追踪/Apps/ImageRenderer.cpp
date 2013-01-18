// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>
#include <ctime>

#include <vector>

#include "VirtualPlatform.h"

int main()
{
    int w = 400, h = 300;
    cout << "Input image's width,height:" << endl;
    cin >> w >> h;
    assert(w > 0 && h > 0);

    std::vector<char> buf;
    buf.resize(w * h * 4);

    setupScene();
    {
        clock_t c = clock();
        onDrawBuffer(&buf[0], w, h, w * 4);
        cout << "cost time : " << (clock() - c) / 1000.f << endl;
    }
    cleanupScene();

    saveImage("scene.png", &buf[0], w, h, w * 4);
    
    {
        cout << "Press any key to continue..." << endl;
        char buf[32];
        cin.getline(buf, sizeof(buf));
        cin.getline(buf, sizeof(buf));
    }
}
