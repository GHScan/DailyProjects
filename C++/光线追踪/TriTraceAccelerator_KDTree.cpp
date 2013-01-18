// vim: fileencoding=gbk
#include "pch.h"

#include <cmath>
#include <ctime>

#include <algorithm>

#include "TriTraceAccelerator_KDTree.h"
#include "TriGeometry.h"
#include "MemoryPool.h"
#include "Traceable.h"
#include "Serialize.h"

class KDTree
{
public:
    KDTree(int maxDepth, int minTriCntPerNode, int splitTest);
    ~KDTree();
    void build(std::vector<Triangle>& tris, const AABB& bounds);
    void destroy();
    bool intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag);
    bool intersectTest(const Ray& r);
    bool intersectSimply(const Ray& r, int intersectFace, float &t);
    int maxDepth() const { return m_maxDepth; }
    int minTriCntPerNode() const { return m_minTriCntPerNode; }
    int splitTest() const { return m_splitTest; }
    int getNodeCount() const { return m_nodePool.getAllocCount(); }

private:
    struct Node
    {
        unsigned int leaf : 1;
        unsigned int axis : 2; // x-0, y-1, z-2
        float val;
        union
        {
            Node* children[2];
            int idxRange[2];
        };
        Node(int _axis, float _val):
            leaf(0), axis(_axis), val(_val)
        {
            assert(axis >= 0 && axis < 3);
            children[0] = children[1] = NULL;
        }
        Node(int idxBegin, int idxEnd):
            leaf(1), axis(0), val(0)
        {
            idxRange[0] = idxBegin;
            idxRange[1] = idxEnd;
        }
    };

private:
    Node* _build(
            const AABB& bounds, const std::vector<Triangle*>& tris, int depth, int allInOneSide);
    bool _intersect(const Node* n, const Ray& r, int intersectFace, float &t, TraceFragment& frag, 
            float t0, float t1, const Vector3 &invRayDir);
    bool _intersectSimply(const Node* n, const Ray& r, int intersectFace, float &t, 
            float t0, float t1, const Vector3 &invRayDir);
    bool _intersectTest(const Node* n, const Ray& r,
            float t0, float t1, const Vector3 &invRayDir);
    float getSplitValue(
            int axis, const AABB& bounds, const std::vector<Triangle*>&tris) const;

private:
    std::vector<Triangle*> m_tris;
    FixSizeMemoryPool<sizeof(Node)> m_nodePool;
    Node *m_root;
    AABB m_bounds;

    const int m_maxDepth;
    const int m_minTriCntPerNode;
    const int m_splitTest;
};
KDTree::KDTree(int maxDepth, int minTriCntPerNode, int splitTest):
    m_root(NULL), m_bounds(Vector3::ZERO, Vector3::ZERO),
    m_maxDepth(maxDepth), m_minTriCntPerNode(minTriCntPerNode), m_splitTest(splitTest)
{
}
KDTree::~KDTree()
{
    destroy();
}
void KDTree::build(std::vector<Triangle>& tris, const AABB& bounds)
{
    destroy();
    assert(m_root == NULL);

    assert(!tris.empty());
    std::vector<Triangle*> _tris;
    for (int i = 0; i < (int)tris.size(); ++i) _tris.push_back(&tris[i]);
    m_bounds = bounds;
    m_root = _build(m_bounds, _tris, 0, 0);
}
KDTree::Node* KDTree::_build(
        const AABB& bounds, const std::vector<Triangle*>& tris, int depth, int allInOneSide)
{
    if (depth >= m_maxDepth || (int)tris.size() <= m_minTriCntPerNode || allInOneSide >= 3) {
        int ibegin = (int)m_tris.size();
        int iend = ibegin + (int)tris.size();
        m_tris.insert(m_tris.end(), tris.begin(), tris.end());
        return new (m_nodePool.malloc()) Node(ibegin, iend);
    }

    int axis = depth % 3;
    float val = getSplitValue(axis, bounds, tris);

    AABB lbounds(bounds), rbounds(bounds);
    lbounds.maxPt[axis] = val;
    rbounds.minPt[axis] = val;

    std::vector<Triangle*> ltris, rtris;

    for (int i = 0; i < (int)tris.size(); ++i) {
        Triangle* tri = tris[i];
        if (tri->p0[axis] <= val || tri->p1[axis] <= val || tri->p2[axis] <= val) {
            ltris.push_back(tri);
        }
        if (tri->p0[axis] > val || tri->p1[axis] > val || tri->p2[axis] > val) {
            rtris.push_back(tri);
        }
    }

    Node *r = new (m_nodePool.malloc()) Node(axis, val);
    if (ltris.empty()) r->children[0] = NULL;
    else r->children[0] = _build(lbounds, ltris, depth + 1, ltris.size() < tris.size() ? 0 : allInOneSide + 1);
    if (rtris.empty()) r->children[1] = NULL;
    else r->children[1] = _build(rbounds, rtris, depth + 1, rtris.size() < tris.size() ? 0 : allInOneSide + 1);
    return r;
}
float KDTree::getSplitValue(
        int axis, const AABB& bounds, const std::vector<Triangle*>&tris) const
{
    assert(axis >= 0 && axis <= 2);
    if (m_splitTest <= 1) {
        return (bounds.minPt[axis] + bounds.maxPt[axis]) * 0.5f;
    }

    if (fequal(bounds.minPt[axis], bounds.maxPt[axis])) {
        return bounds.minPt[axis];
    }

    // 注意这个static! 是为了缓解大型网络构建时的内存瓶颈
    static std::vector<float> minArray;
    static std::vector<float> maxArray;
    minArray.resize(tris.size());
    maxArray.resize(tris.size());
    for (int i = 0; i < (int)tris.size(); ++i) {
        Triangle* tr = tris[i];
        minArray[i] = std::min(std::min(tr->p0[axis], tr->p1[axis]), tr->p2[axis]);
        maxArray[i] = std::max(std::max(tr->p0[axis], tr->p1[axis]), tr->p2[axis]);
    }
    std::sort(minArray.begin(), minArray.end());
    std::sort(maxArray.begin(), maxArray.end());

    float step = (bounds.maxPt[axis] - bounds.minPt[axis]) / (m_splitTest + 1);
    float bestCost = MAX_FLOAT;
    float bestVal = 0;
    float val = bounds.minPt[axis] + step;
    for (int i = 0; i < m_splitTest; ++i, val += step) {
        int left = int(std::upper_bound(minArray.begin(), minArray.end(), val) - minArray.begin());
        int right = int(maxArray.end() - std::lower_bound(maxArray.begin(), maxArray.end(), val));
        float pl = (val - bounds.minPt[axis]) / (bounds.maxPt[axis] - bounds.minPt[axis]);
        if (pl > 1) pl = 1;
        assert(pl <= 1 && pl >= 0);
        float pr = 1 - pl;
        float cost = pl * left + pr * right;
        if (cost < bestCost) {
            bestCost = cost;
            bestVal = val;
        }
    }
    return bestVal;
}
void KDTree::destroy()
{
    m_tris.clear();
    std::vector<Node*> nodes;
    if (m_root != NULL) nodes.push_back(m_root);
    while (!nodes.empty()) {
        Node* p = nodes.back();
        nodes.pop_back();
        if (!p->leaf) {
            if (p->children[0] != NULL) {
                nodes.push_back(p->children[0]);
            }
            if (p->children[1] != NULL) {
                nodes.push_back(p->children[1]);
            }
        }
        p->~Node();
        m_nodePool.free(p);
    }
    m_root = NULL;
}
bool KDTree::_intersect(const Node* n,
        const Ray& r, int intersectFace, float &t, TraceFragment& frag, 
        float t0, float t1, const Vector3& invRayDir)
{
    if (n->leaf) {
        Vector2 ab;
        Triangle *tri = NULL;
        for (int i = n->idxRange[0]; i < n->idxRange[1]; ++i) {
            Triangle* _tri = m_tris[i];
            float _t;
            E_IntersectCase ic = ::intersect(r, _tri->plane, _t);
            if (intersectFace & ic) {
                if (_t >= t0 && _t <= t1 && _t < t) {
                    Vector3 pt = r.getPoint(_t);
                    Vector2 _ab;
                    if (isPointInTriangle(pt, *_tri, _ab)) {
                        t = _t;
                        ab = _ab;
                        tri = _tri;
                        frag.pos = pt;
                    }
                }
            }
        }
        if (tri != NULL) {
            interpolateNormUV(frag, *tri, ab);
            updateTangentSpace(frag, *tri);
            return true;
        }
        return false;
    }

    Node* nextNodes[2] = {0};
    int nextNodeCnt = 0;
    float mid = 0;
    switch (n->axis) {
        case 0:
            {
                int flag = 
                    ((r.pt.x + r.dir.x * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.x + r.dir.x * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.x) * invRayDir.x;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.x) * invRayDir.x;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
        case 1:
            {
                int flag = 
                    ((r.pt.y + r.dir.y * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.y + r.dir.y * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.y) * invRayDir.y;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.y) * invRayDir.y;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
        case 2:
            {
                int flag = 
                    ((r.pt.z + r.dir.z * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.z + r.dir.z * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.z) * invRayDir.z;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.z) * invRayDir.z;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
    }

    if (nextNodeCnt == 1) {
        if (nextNodes[0] != NULL && t0 < t &&
                _intersect(nextNodes[0], r, intersectFace, t, frag, t0, t1, invRayDir)) return true;
    }
    else {
        assert(nextNodeCnt == 2);
        if (nextNodes[0] != NULL && t0 < t &&
                _intersect(nextNodes[0], r, intersectFace, t, frag, t0, mid, invRayDir)) return true;
        if (nextNodes[1] != NULL && mid < t &&
                _intersect(nextNodes[1], r, intersectFace, t, frag, mid, t1, invRayDir)) return true;
    }
    return false;
}
bool KDTree::_intersectSimply(const Node* n,
        const Ray& r, int intersectFace, float &t, 
        float t0, float t1, const Vector3& invRayDir)
{
    if (n->leaf) {
        bool b = false;
        for (int i = n->idxRange[0]; i < n->idxRange[1]; ++i) {
            Triangle* _tri = m_tris[i];
            float _t;
            E_IntersectCase ic = ::intersect(r, _tri->plane, _t);
            if (intersectFace & ic) {
                if (_t >= t0 && _t <= t1 && _t < t) {
                    Vector3 pt = r.getPoint(_t);
                    Vector2 _ab;
                    if (isPointInTriangle(pt, *_tri, _ab)) {
                        t = _t;
                        b = true;
                    }
                }
            }
        }
        return b;
    }

    Node* nextNodes[2] = {0};
    int nextNodeCnt = 0;
    float mid = 0;
    switch (n->axis) {
        case 0:
            {
                int flag = 
                    ((r.pt.x + r.dir.x * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.x + r.dir.x * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.x) * invRayDir.x;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.x) * invRayDir.x;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
        case 1:
            {
                int flag = 
                    ((r.pt.y + r.dir.y * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.y + r.dir.y * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.y) * invRayDir.y;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.y) * invRayDir.y;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
        case 2:
            {
                int flag = 
                    ((r.pt.z + r.dir.z * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.z + r.dir.z * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.z) * invRayDir.z;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.z) * invRayDir.z;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
    }

    if (nextNodeCnt == 1) {
        if (nextNodes[0] != NULL && t0 < t &&
                _intersectSimply(nextNodes[0], r, intersectFace, t, t0, t1, invRayDir)) return true;
    }
    else {
        assert(nextNodeCnt == 2);
        if (nextNodes[0] != NULL && t0 < t &&
                _intersectSimply(nextNodes[0], r, intersectFace, t, t0, mid, invRayDir)) return true;
        if (nextNodes[1] != NULL && mid < t &&
                _intersectSimply(nextNodes[1], r, intersectFace, t, mid, t1, invRayDir)) return true;
    }
    return false;
}
bool KDTree::_intersectTest(const Node* n,
        const Ray& r, 
        float t0, float t1, const Vector3& invRayDir)
{
    if (n->leaf) {
        for (int i = n->idxRange[0]; i < n->idxRange[1]; ++i) {
            Triangle* _tri = m_tris[i];
            float _t;
            if (::intersect(r, _tri->plane, _t) != EIC_none) {
                if (_t >= t0 && _t <= t1) {
                    Vector3 pt = r.getPoint(_t);
                    Vector2 _ab;
                    if (isPointInTriangle(pt, *_tri, _ab)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    Node* nextNodes[2] = {0};
    int nextNodeCnt = 0;
    float mid = 0;
    switch (n->axis) {
        case 0:
            {
                int flag = 
                    ((r.pt.x + r.dir.x * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.x + r.dir.x * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.x) * invRayDir.x;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.x) * invRayDir.x;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
        case 1:
            {
                int flag = 
                    ((r.pt.y + r.dir.y * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.y + r.dir.y * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.y) * invRayDir.y;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.y) * invRayDir.y;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
        case 2:
            {
                int flag = 
                    ((r.pt.z + r.dir.z * t0 <= n->val ? 1 : 0) << 1) | 
                    (r.pt.z + r.dir.z * t1 <= n->val ? 1 : 0);
                switch (flag) {
                    case 0:
                        nextNodes[0] = n->children[1];
                        nextNodeCnt = 1;
                        break;
                    case 1:
                        nextNodes[0] = n->children[1];
                        nextNodes[1] = n->children[0];
                        mid = (n->val - r.pt.z) * invRayDir.z;
                        nextNodeCnt = 2;
                        break; 
                    case 2:
                        nextNodes[0] = n->children[0];
                        nextNodes[1] = n->children[1];
                        mid = (n->val - r.pt.z) * invRayDir.z;
                        nextNodeCnt = 2;
                        break; 
                    case 3:
                        nextNodes[0] = n->children[0];
                        nextNodeCnt = 1;
                        break;
                }
            }
            break;
    }

    if (nextNodeCnt == 1) {
        if (nextNodes[0] != NULL &&
                _intersectTest(nextNodes[0], r, t0, t1, invRayDir)) return true;
    }
    else {
        assert(nextNodeCnt == 2);
        if (nextNodes[0] != NULL &&
                _intersectTest(nextNodes[0], r, t0, mid, invRayDir)) return true;
        if (nextNodes[1] != NULL && 
                _intersectTest(nextNodes[1], r, mid, t1, invRayDir)) return true;
    }
    return false;
}
bool KDTree::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    float ts[2] = {0};
    if (!rayIntersectAABB(r, m_bounds, ts)) return false;
    if (ts[0] >= t) return false;

    Vector3 invRayDir;
    if (!fequal(r.dir.x, 0)) invRayDir.x = 1 / r.dir.x;
    if (!fequal(r.dir.y, 0)) invRayDir.y = 1 / r.dir.y;
    if (!fequal(r.dir.z, 0)) invRayDir.z = 1 / r.dir.z;

    return _intersect(m_root, r, intersectFace, t, frag, ts[0], ts[1], invRayDir);
}
bool KDTree::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    float ts[2] = {0};
    if (!rayIntersectAABB(r, m_bounds, ts)) return false;
    if (ts[0] >= t) return false;

    Vector3 invRayDir;
    if (!fequal(r.dir.x, 0)) invRayDir.x = 1 / r.dir.x;
    if (!fequal(r.dir.y, 0)) invRayDir.y = 1 / r.dir.y;
    if (!fequal(r.dir.z, 0)) invRayDir.z = 1 / r.dir.z;

    return _intersectSimply(m_root, r, intersectFace, t, ts[0], ts[1], invRayDir);
}

bool KDTree::intersectTest(const Ray& r)
{
    float ts[2] = {0};
    if (!rayIntersectAABB(r, m_bounds, ts)) return false;

    Vector3 invRayDir;
    if (!fequal(r.dir.x, 0)) invRayDir.x = 1 / r.dir.x;
    if (!fequal(r.dir.y, 0)) invRayDir.y = 1 / r.dir.y;
    if (!fequal(r.dir.z, 0)) invRayDir.z = 1 / r.dir.z;

    return _intersectTest(m_root, r, ts[0], ts[1], invRayDir);
}
//----------------------------------------
// TriTraceAccelerator_KDTree
//----------------------------------------
TriTraceAccelerator_KDTree::TriTraceAccelerator_KDTree():
    m_kdtree(new KDTree(4, 4, 3))
{
}
TriTraceAccelerator_KDTree::~TriTraceAccelerator_KDTree()
{
    sdelete(m_kdtree);
}
void TriTraceAccelerator_KDTree::rebuild(const SubMesh* sub, const Matrix4x4& worldView)
{
    TriTraceAccelerator_Base::rebuild(sub, worldView);

    clock_t c = clock();
    m_kdtree->build(m_tris, m_aabb);
    if (m_tris.size() > 10000) // 10000个三角形以上开销才比较大
    {
        cout << "build kd-tree :"  << endl
            << "    time: " << (clock() - c) / 1000.f << endl
            << "    triangle count: " << m_tris.size() << endl
            << "    node count: " << m_kdtree->getNodeCount() << endl;
    }
}
bool TriTraceAccelerator_KDTree::intersect(const Ray& r, int intersectFace, float &t, TraceFragment& frag)
{
    const Material* oldMat = frag.mat;
    frag.mat = m_mat;
    if (!m_kdtree->intersect(r, intersectFace, t, frag)) {
        frag.mat = oldMat;
        return false;
    }
    return true;
}
bool TriTraceAccelerator_KDTree::intersectSimply(const Ray& r, int intersectFace, float &t)
{
    return m_kdtree->intersectSimply(r, intersectFace, t);
}
bool TriTraceAccelerator_KDTree::intersectTest(const Ray& r)
{
    return m_kdtree->intersectTest(r);
}
TriTraceAccelerator_Base* TriTraceAccelerator_KDTree::clone() const
{
    TriTraceAccelerator_KDTree *p = new TriTraceAccelerator_KDTree();
    sdelete(p->m_kdtree);
    p->m_kdtree = new KDTree(m_kdtree->maxDepth(), 
            m_kdtree->minTriCntPerNode(), 
            m_kdtree->splitTest());
    return p;
}
void TriTraceAccelerator_KDTree::printStream(std::ostream& so) const
{
    StreamBlockWriter w("KDTree", so);
    w.write("maxDepth", m_kdtree->maxDepth());
    w.write("minTriCntPerNode", m_kdtree->minTriCntPerNode());
    w.write("splitTest", m_kdtree->splitTest());
}
void TriTraceAccelerator_KDTree::scanStream(std::istream& si)
{
    StreamBlockReader r("KDTree", si);
    int maxDepth = 4, minTriCntPerNode = 4, splitTest = 3;
    if (!r.read("maxDepth", &maxDepth)) assert(0);
    if (!r.read("minTriCntPerNode", &minTriCntPerNode)) assert(0);
    if (!r.read("splitTest", &splitTest)) assert(0);

    sdelete(m_kdtree);
    m_kdtree = new KDTree(maxDepth, minTriCntPerNode, splitTest);
}
