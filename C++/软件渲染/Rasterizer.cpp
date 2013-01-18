// vim: fileencoding=gbk

#include "pch.h"

#include <cmath>
#include <cassert>

#include <algorithm>

#include "Rasterizer.h"


int Rasterizer::width() const { return m_w; }
int Rasterizer::height() const { return m_h; }
int Rasterizer::pitch() const { return m_pitch; }

Rasterizer::Rasterizer(char *buf, int w, int h, int pitch,
        float *zbuf, Sampler* sampler):
    m_buf(buf), m_w(w), m_h(h), m_pitch(pitch), m_zbuf(zbuf), m_sampler(sampler)
{
    assert(m_buf != NULL && m_w > 0 && m_h > 0 && m_pitch >= 4);
}
void Rasterizer::drawLine(int x0, int y0, int x1, int y1, int rgb)
{
    // 裁剪
    if (x0 > x1) {
        std::swap(x0, x1);
        std::swap(y0, y1);
    }
    if (x0 >= m_w || x1 < 0) return;
    if (std::min(y0, y1) >= m_h || std::max(y0, y1) < 0) return;

    int xDiff = x1 - x0 + 1;
    int yDiff = abs(y1 - y0) + 1;
    char *buf = m_buf + y0 * m_pitch;
    char *bufEnd = m_buf + m_h * m_pitch;
    int yAdv = y1 == y0 ? 0 : (y1 > y0 ? m_pitch : -m_pitch);
    if (xDiff >= yDiff) {
        int score = xDiff;
        bool drawn = false;
        for (int x = x0; x <= x1; ++x) {
            if (x >= 0 && x < m_w && buf >= m_buf && buf < bufEnd) {
                ((int*)buf)[x] = rgb;
                drawn = true;
            }
            else {
                if (drawn) break;
            }
            score -= yDiff;
            if (score <= 0) {
                score += xDiff;
                buf += yAdv;
            }
        }
    }
    else {
        int score = yDiff;
        int x = x0;
        bool drawn = false;
        for (int i = 0; i < yDiff; ++i) {
            if (x >= 0 && x < m_w && buf >= m_buf && buf < bufEnd) {
                ((int*)buf)[x] = rgb;
                drawn = true;
            }
            else {
                if (drawn) break;
            }
            buf += yAdv;
            score -= xDiff;
            if (score <= 0) {
                score += yDiff;
                x += 1;
            }
        }
    }
}

void Rasterizer::drawPoint(int x, int y, int rgb)
{
    assert(x >= 0 && x < m_w);
    assert(y >= 0 && y < m_h);
    ((int*)(m_buf + y * m_pitch))[x] = rgb;
}

void Rasterizer::drawFrameTriangle(
        int x0, int y0, int x1, int y1, int x2, int y2,
        int rgb)
{
    drawLine(x0, y0, x1, y1, rgb);
    drawLine(x0, y0, x2, y2, rgb);
    drawLine(x1, y1, x2, y2, rgb);
}
