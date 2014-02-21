#include "pch.h" 

#include <list>
#include <functional>

int g_curTime;
static int mytime() {
    return g_curTime;
}

class Timer {
public:
    Timer(): m_lastUpdateTime(mytime()), m_curTick(0){}
    void add(int interval, function<void()> f) {
        interval += mytime() - m_lastUpdateTime;
        Item item = {m_curTick + interval, f};
        int idx = 0;
        for (int t = interval; t > 0; t >>= 1, ++idx);
        if (interval > 0 && ((interval - 1) & interval) == 0) m_lists[idx - 1].push_back(item);
        else m_lists[idx].push_back(item);
    }
    void update() {
        int oldTick = m_curTick;
        int curTime = mytime();
        m_curTick += curTime - m_lastUpdateTime;
        m_lastUpdateTime = curTime;

        int maxIdx = 0;
        for (int diff = oldTick ^ m_curTick; diff > 0; ++maxIdx, diff >>= 1);

        for (; maxIdx > 0; --maxIdx) {
            int gate = 1 << (maxIdx - 1);
            for (auto iter = m_lists[maxIdx].begin(); iter != m_lists[maxIdx].end(); ) {
                if (iter->endTick - m_curTick <= gate) {
                    m_lists[maxIdx - 1].push_back(*iter);
                    iter = m_lists[maxIdx].erase(iter);
                } else ++iter;
            }
        }
        for (auto iter = m_lists[0].begin(); iter != m_lists[0].end(); ) {
            if (iter->endTick - m_curTick <= 0) {
                iter->f();
                iter = m_lists[0].erase(iter);
            } else ++iter;
        }
    }
private:
    struct Item {
        int endTick;
        function<void()> f;
    };
private:
    int m_lastUpdateTime;
    int m_curTick;
    list<Item> m_lists[32];
};

int main() {
    srand(time(nullptr));

    const int TIME = 100;

    vector<int> rints(32);
    generate(rints.begin(), rints.end(), [](){ return rand() % TIME; });

    Timer timer;

    puts("******************** step 1");
    for (auto i : rints) {
        timer.add(i, [i](){ printf("callback: interval=%d, time=%d\n", i, mytime());});
    }
    for (; g_curTime < TIME; ++g_curTime) {
        timer.update();
    }

    puts("******************** step n");
    for (auto i : rints) {
        timer.add(i, [i](){ printf("callback: interval=%d, time=%d\n", i, mytime() - TIME);});
    }
    for (; g_curTime < 2 * TIME; g_curTime += min(rand() % 32, 2 * TIME - g_curTime)) {
        timer.update();
    }
}
