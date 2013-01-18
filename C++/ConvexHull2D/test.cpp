// vim:fileencoding=gbk

#include "pch.h"

#include <ctime>
#include <cassert>
#include <cmath>

#include <exception>
#include <vector>
#include <algorithm>

#include <gl/glut.h>

const int WIDTH = 400, HEIGHT = 300;

inline bool fequal(float a, float b)
{
    return fabs(a - b) <= 0.0001f;
}

struct Vector2
{
    float x, y;
    Vector2(float _x, float _y): x(_x), y(_y){}
    Vector2& operator -= (const Vector2& o) 
    {
        x -= o.x; y -= o.y;
        return *this;
    }
    Vector2 operator - (const Vector2& o) const
    {
        return Vector2(*this) -= o;
    }
    bool operator == (const Vector2& o) const
    {
        return fequal(x, o.x) && fequal(y, o.y);
    }
    bool operator < (const Vector2& o) const
    {
        if (y == o.y) {
            return x < o.x;
        }
        return y > o.y;
    }
    float dot(const Vector2& o) const { return x * o.x + y * o.y; }
    float cross(const Vector2& o) const { return x * o.y - y * o.x; }
    float length() const { return sqrt(dot(*this));  }
    static Vector2 AXIS_X;
};
Vector2 Vector2::AXIS_X(1, 0);

class ConvexHull
{
public:
    void gen(int n);
    void drawPoints();
    void drawConvexHull();

private:
    void genPoints(int n);
    void calcConvexHull();

private:
    std::vector<Vector2> m_points;
    std::vector<Vector2> m_convexHull;
};

void ConvexHull::gen(int n)
{
    genPoints(n);
    calcConvexHull();
}
void ConvexHull::drawPoints()
{
    const float RADIUS_W = 5.0 / WIDTH, RADIUS_H = 5.0 / HEIGHT;

    glColor3f(1, 1, 1);
    for (int i = 0; i < (int)m_points.size(); ++i) {
        const Vector2& pt = m_points[i];
        glRectf(pt.x - RADIUS_W, pt.y - RADIUS_H, pt.x + RADIUS_W, pt.y + RADIUS_H);
    }
}
void ConvexHull::drawConvexHull()
{
    glColor3f(1, 0, 0);
    glBegin(GL_LINE_LOOP);
    for (int i = 0; i < (int)m_convexHull.size(); ++i) {
        const Vector2& pt(m_convexHull[i]);
        glVertex2f(pt.x, pt.y);
    }
    glEnd();
}
void ConvexHull::genPoints(int n)
{
    m_points.clear();
    for (int i = 0; i < n; ++i) {
        float x = (rand() % (WIDTH - 20) + 10) / float(WIDTH) * 2 - 1;
        float y = 1 - (rand() % (HEIGHT - 20) + 10) / float(HEIGHT) * 2;
        m_points.push_back(Vector2(x, y));
    }
}

void ConvexHull::calcConvexHull()
{
    m_convexHull.clear();

    std::sort(m_points.begin(), m_points.end());
    m_points.erase(std::unique(m_points.begin(), m_points.end()), m_points.end());

    struct PointInfo
    {
        Vector2 pt;
        float cosA;
        float dis;
    };
    std::vector<PointInfo> points;
    for (int i = 1; i < (int)m_points.size(); ++i) {
        PointInfo info = {m_points[i]};
        info.dis = (info.pt - m_points[0]).length();
        info.cosA = (info.pt - m_points[0]).dot(Vector2::AXIS_X) / info.dis;
        points.push_back(info);
    }

    struct Comparer
    {
        bool operator () (const PointInfo& l, const PointInfo& r) const
        {
            if (fequal(l.cosA, r.cosA)) {
                return l.dis > r.dis;
            }
            return l.cosA < r.cosA;
        }
    };
    std::sort(points.begin(), points.end(), Comparer());

    int sz = (int)points.size();
    for (int i = 0; i < (int)points.size() - 1; ++i) {
        if (fequal(points[i].cosA, points[i + 1].cosA)) {
            points.erase(points.begin() + i + 1);
        }
    }
    cout << "trim same angle:" << sz - points.size() << endl;

    m_convexHull.push_back(m_points[0]);
    m_convexHull.push_back(points[0].pt);
    m_convexHull.push_back(points[1].pt);
    for (int i = 2; i < (int)points.size(); ++i) {
        for (;;) {
            const Vector2& pt0 = m_convexHull[m_convexHull.size() - 2];
            const Vector2& pt1 = m_convexHull[m_convexHull.size() - 1];
            const Vector2& pt2 = points[i].pt;
            if ((pt2 - pt1).cross(pt0 - pt1) > 0.0001) {
                break;
            }
            m_convexHull.pop_back();
        }
        m_convexHull.push_back(points[i].pt);
    }
}

ConvexHull *g_conexHull = NULL;

void detectGLError()
{
    GLenum err = glGetError();
    if (err != GL_NO_ERROR) {
        throw std::exception((const char *)gluErrorString(err));
    }
}

void onSetup()
{
    glClearColor(0, 0, 0, 0);

    srand((unsigned int)time(NULL));

    g_conexHull = new ConvexHull();
    g_conexHull->gen(60);
}

void onCleanup()
{
    delete g_conexHull;
    g_conexHull = NULL;
}

void onDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT);

    g_conexHull->drawPoints();
    g_conexHull->drawConvexHull();

    glutSwapBuffers();
    detectGLError();
}

void onIdle()
{
    glutPostRedisplay();
}

void onReshape(int w, int h)
{
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    float fw = float(w) / WIDTH, fh = float(h) / HEIGHT;
    gluOrtho2D(-fw, fw, -fh, fh);
    glMatrixMode(GL_MODELVIEW);
}

void onKeyUp(unsigned char k, int x, int y)
{
    if (k == 0x1b) {
        onCleanup();
        exit(0);
    }
    else if (k == 'r') {
        int n = rand() % 100 + 5;
        if (rand() % 3) {
            n = rand() % 1000 + 10;
        }
        cout << "gen n : " << n << endl;
        g_conexHull->gen(n);
    }
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(WIDTH, HEIGHT);

    glutCreateWindow("GLUT App");

    glutDisplayFunc(onDisplay);
    glutIdleFunc(onIdle);
    glutReshapeFunc(onReshape);
    glutKeyboardUpFunc(onKeyUp);

    onSetup();

    try {
        glutMainLoop();
    } catch(const std::exception &e) {
        cout << "exception : " << e.what() << endl;
    }
}
