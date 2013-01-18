// vim:fileencoding=gbk

#include "pch.h"

#include <ctime>
#include <cmath>
#include <cassert>

#include <exception>
#include <vector>
#include <set>
#include <algorithm>
#include <functional>

#include <gl/glut.h>

float EPSILON = 0.0001f;

inline bool fequal(float a, float b)
{
    return fabs(a - b) < EPSILON;
}

struct Vector3
{
    explicit Vector3(float _x = 0, float _y = 0, float _z = 0): 
        x(_x), y(_y), z(_z){}

    Vector3& operator -= (const Vector3& o)
    {
        x -= o.x, y -= o.y, z -= o.z;
        return *this;
    }
    Vector3 operator - (const Vector3& o) const
    {
        return Vector3(*this) -= o;
    }

    Vector3& operator += (const Vector3& o)
    {
        x += o.x, y += o.y, z += o.z;
        return *this;
    }
    Vector3 operator + (const Vector3& o) const
    {
        return Vector3(*this) += o;
    }

    Vector3& operator *= (float f)
    {
        x *= f, y *= f, z *= f;
        return *this;
    }
    Vector3 operator * (float f) const
    {
        return Vector3(*this) *= f;
    }

    Vector3& operator /= (float f)
    {
        assert(!fequal(f, 0));
        return *this *= 1 / f;
    }
    Vector3 operator / (float f) const
    {
        return Vector3(*this) /= f;
    }
    
    float lengthSqr() const
    {
        return x * x + y * y + z * z;
    }
    float length() const
    {
        return sqrt(lengthSqr());
    }
    Vector3 normalize() const
    {
        return Vector3(*this) /= length();
    }

    float dot(const Vector3& o) const 
    {
        return x * o.x + y * o.y + z * o.z;
    }
    Vector3 cross(const Vector3& o) const
    {
        return Vector3(y * o.z - z * o.y, 
                z * o.x - x * o.z,
                x * o.y - y * o.x);
    }

    bool operator == (const Vector3& o) const
    {
        return fequal(x, o.x) && fequal(y, o.y) && fequal(z, o.z);
    }
    bool operator != (const Vector3& o) const
    {
        return !(*this == o);
    }
    bool operator < (const Vector3& o) const
    {
        if (fequal(x, o.x)) {
            if (fequal(y, o.y)) {
                return z < o.z;
            }
            return y < o.y;
        }
        return x < o.x;
    }

    static Vector3 AXIS_X;
    static Vector3 AXIS_Y;
    static Vector3 AXIS_Z;

    float x, y, z;
};
Vector3 Vector3::AXIS_X = Vector3(1, 0, 0);
Vector3 Vector3::AXIS_Y = Vector3(0, 1, 0);
Vector3 Vector3::AXIS_Z = Vector3(0, 0, 1);

struct Triangle
{
    int p0, p1, p2;
    const Vector3 *pts;
    Vector3 norm;
    Triangle(int _p0, int _p1, int _p2, const Vector3* _pts):
        p0(_p0), p1(_p1), p2(_p2), pts(_pts)
    {
        const Vector3& v10(pts[p0] - pts[p1]);
        const Vector3& v12(pts[p2] - pts[p1]);
        norm = v12.cross(v10).normalize();
    }
    float disOfPoint(const Vector3& o) const
    {
        return (o - pts[p0]).dot(norm);
    }
    int edge(int i) const
    {
        const int *p = &p0;
        return (p[i] << 16) | p[(i + 1) % 3];
    }
    int redge(int i) const
    {
        int e = edge(i);
        return (e << 16) | ((e >> 16) & 0xffff);
    }
};

class ConvexHull3D
{
public:
    void gen(int len);
    void drawPoints();
    void drawHull();

private:
    bool calcHull();

private:
    std::vector<Vector3> m_points;
    std::vector<Triangle> m_tris;
};

void ConvexHull3D::gen(int len)
{
    do
    {
        m_points.clear();
        while ((int)m_points.size() < len) {
            Vector3 pt(
                    rand() % 10000 / 1000.f - 5, 
                    rand() % 10000 / 1000.f - 5, 
                    rand() % 10000 / 1000.f - 5);
            m_points.push_back(pt);
        }
    } while(!calcHull());
}

void ConvexHull3D::drawPoints()
{
    glColor3f(1, 0, 0);
    glBegin(GL_POINTS);
    for (int i = 0; i < (int)m_points.size(); ++i) {
        const Vector3& pt = m_points[i];
        glVertex3f(pt.x, pt.y, pt.z);
    }
    glEnd();
}

void ConvexHull3D::drawHull()
{
    glColor3f(1, 1, 1);
    glBegin(GL_TRIANGLES);
    for (int i = 0; i < (int)m_tris.size(); ++i) {
        const Triangle& tri = m_tris[i];
        glNormal3f(tri.norm.x, tri.norm.y, tri.norm.z);
        glVertex3f(m_points[tri.p0].x, m_points[tri.p0].y, m_points[tri.p0].z);
        glVertex3f(m_points[tri.p1].x, m_points[tri.p1].y, m_points[tri.p1].z);
        glVertex3f(m_points[tri.p2].x, m_points[tri.p2].y, m_points[tri.p2].z);
    }
    glEnd();
}

bool ConvexHull3D::calcHull()
{
    std::sort(m_points.begin(), m_points.end());
    m_points.erase(std::unique(m_points.begin(), m_points.end()), m_points.end());
    if (m_points.size() < 4) return false;

    // the first 3 point is in the same line
    {
        Vector3 v01(m_points[1] - m_points[0]);
        Vector3 v02(m_points[2] - m_points[0]);
        if (fequal(fabs(v01.dot(v02)),
                    v01.length() * v02.length())) {
            return false;
        }
    }

    // the fourth point is in the same plane
    {
        Triangle tri(0, 1, 2, &m_points[0]);
        float dis = tri.disOfPoint(m_points[3]);
        if (fequal(dis, 0)) return false;
        if (dis > 0) {
            std::swap(m_points[1], m_points[2]);
        }
    }

    std::vector<Triangle> tris;
    tris.push_back(Triangle(0, 1, 2, &m_points[0]));
    tris.push_back(Triangle(0, 3, 1, &m_points[0]));
    tris.push_back(Triangle(0, 2, 3, &m_points[0]));
    tris.push_back(Triangle(3, 2, 1, &m_points[0]));

    struct Front:
        std::unary_function<Triangle, bool>
    {
        Front(const Vector3& _pt): pt(_pt){}
        bool operator () (const Triangle& tri) const
        {
            return tri.disOfPoint(pt) > EPSILON;
        }
        Vector3 pt;
    };

    for (int i = 4; i < (int)m_points.size(); ++i) {
        std::vector<Triangle>::iterator iter = std::remove_if(tris.begin(), tris.end(), Front(m_points[i]));
        if (iter == tris.end()) {
            continue;
        }

        tris.erase(iter, tris.end());

        std::set<int> edges;
        for (int j = 0; j < (int)tris.size(); ++j) {
            const Triangle& tri = tris[j];
            for (int k = 0; k < 3; ++k) {
                int re = tri.redge(k);
                if (edges.count(re)) {
                    edges.erase(re);
                    continue;
                }
                edges.insert(tri.edge(k));
            }
        }

        for (std::set<int>::const_iterator iter = edges.begin();
                iter != edges.end();
                ++iter) {
            int e = *iter;
            tris.push_back(Triangle(e & 0xffff, (e >> 16) & 0xffff, i, &m_points[0]));
        }
    }

    for (int i = 0; i < (int)m_points.size(); ++i) {
        assert(std::remove_if(tris.begin(), tris.end(), Front(m_points[i])) == tris.end());
    }

    m_tris = tris;

    return true;
}

ConvexHull3D *g_hull = NULL;

void changeRenderMode()
{
    static int mode = 0;
    switch (mode) {
        case 0:
            glDisable(GL_LIGHTING);
            glDisable(GL_LIGHT0);
            glDisable(GL_CULL_FACE);
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
            break;
        case 1:
            glDisable(GL_LIGHTING);
            glDisable(GL_LIGHT0);
            glEnable(GL_CULL_FACE);
            glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);

            glCullFace(GL_BACK);
            break;
        case 2:
            glEnable(GL_LIGHTING);
            glEnable(GL_LIGHT0);
            glEnable(GL_CULL_FACE);
            glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);

            glCullFace(GL_BACK);

            glLightfv(GL_LIGHT0, GL_AMBIENT, (float*)&Vector3(0.2f, 0.2f, 0.2f));
            glLightfv(GL_LIGHT0, GL_DIFFUSE, (float*)&Vector3(0.8f, 0.8f, 0.8f));
            {
                glPushMatrix();
                glLoadIdentity();

                float dir[] = {10, 10, 0, 1};
                glLightfv(GL_LIGHT0, GL_POSITION, dir);

                glPopMatrix();
            }

            glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, (float*)&Vector3(1, 1, 1));
            break;
        default:
            break;
    }
    mode = (mode + 1) % 3;
}

void detectGLError()
{
    GLenum err = glGetError();
    if (err != GL_NO_ERROR) {
        throw std::exception((const char *)gluErrorString(err));
    }
}

void onSetup()
{
    srand(time(NULL));

    glClearColor(0, 0, 0, 0);
    changeRenderMode();

    g_hull = new ConvexHull3D();
    g_hull->gen(4);
}

void onCleanup()
{
    delete g_hull;
    g_hull = NULL;
}

void onDisplay()
{
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    g_hull->drawPoints();
    g_hull->drawHull();

    glutSwapBuffers();
    detectGLError();
}

void onIdle()
{
    glRotatef(3.14/4, 0, 0, 1);

    glutPostRedisplay();
}

void onReshape(int w, int h)
{
    if (w < 1) w = 1;
    if (h < 1) h = 1;
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(60, w / float(h), 0.5f, 25);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glTranslatef(0, 0, -15);
}

void onKeyUp(unsigned char k, int x, int y)
{
    switch (k) {
    case 0x1b:
        onCleanup();
        exit(0);
        break;
    case 'r':
        {
            int len = rand() % 3 ? 
                    (rand() % 6) : 
                    (rand() % 1000);

            clock_t c = clock();
            g_hull->gen(len + 4);
            printf("points (%d), time (%f)\n", len + 4, (clock() - c) / float(CLOCKS_PER_SEC));
        }
        break;
    case 'm':
        changeRenderMode();
        break;

    default:
        break;
    }
}

int main(int argc, char *argv[])
{
    glutInit(&argc, argv);
    glutInitDisplayMode(GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH);
    glutInitWindowPosition(100, 100);
    glutInitWindowSize(400, 300);

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
