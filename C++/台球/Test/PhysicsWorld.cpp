#include "StdAfx.h"

#include <limits>

#include "PhysicsWorld.h"

namespace Physics
{
////////////////////////////////////////////////////////////////////////////
Line::Line(const core::vector2df& dir, const core::vector2df& pt, bool normalizeDir):
m_dir(dir), m_pt(pt)
{
    if (normalizeDir) m_dir.normalize();
    assert(core::equals(m_dir.getLength(), 1.0f));
}
const core::vector2df& Line::getDir() const { return m_dir; }
const core::vector2df& Line::getPoint() const { return m_pt; }
PrimitiveType Line::_getType() { return PT_Line; }
PrimitiveType Line::getType() const { return _getType(); }
////////////////////////////////////////////////////////////////////////////
Circle::Circle(float radius, float mass, 
       const core::vector2df& pos,
       const core::vector2df& vec):
m_radius(radius), m_mass(mass), m_pos(pos), m_vec(vec) { }
float Circle::getRadius() const { return m_radius; }
float Circle::getMass() const { return m_mass; }
const core::vector2df& Circle::getPos() const { return m_pos; }
const core::vector2df& Circle::getVec() const { return m_vec; }
void Circle::setPos(const core::vector2df& pos) { m_pos = pos; }
void Circle::setVec(const core::vector2df& vec) { m_vec = vec; }
PrimitiveType Circle::_getType() { return PT_Circle; }
PrimitiveType Circle::getType() const { return _getType(); }
void Circle::update(float elapse)
{
    m_pos += m_vec * elapse;
}

void Circle::applyMotionFriction(float fricion, float elapse)
{
    if (core::equals(m_mass, 0.0f)) return; // 0表示质量很大的物体, 当然不受动摩擦影响
    float speed = m_vec.getLength();
    float speedDec = fricion / m_mass * elapse;
    if (speed <= speedDec) m_vec = core::vector2df();
    else m_vec *= (speed - speedDec) / speed;
}
////////////////////////////////////////////////////////////////////////////
static float testCollide_Circle_Circle(Primitive *p1, Primitive *p2)
{
    assert(p1->getType() == PT_Circle && p2->getType() == PT_Circle);

    float defaultRet = -std::numeric_limits<float>::max();
    
    Physics::Circle* circle0 = static_cast<Physics::Circle*>(p1);
    Physics::Circle* circle1 = static_cast<Physics::Circle*>(p2);

    core::vector2df v01 = circle0->getVec() - circle1->getVec();
    core::vector2df p01 = circle0->getPos() - circle1->getPos();

    float rr2 = circle0->getRadius() + circle1->getRadius();
    rr2 *= rr2;

    float a = v01.getLengthSQ();
    float b = 2 * v01.dotProduct(p01);
    float c = p01.getLengthSQ() - rr2;
    if (core::equals(a, 0.0f)) return defaultRet;

    float b2p4ac = b * b - 4 * a * c;
    if (b2p4ac < 0) return defaultRet; // 不能相交
    float sqrt_b2p4ac = sqrtf(b2p4ac);
    
    float tMin = (-b - sqrt_b2p4ac) / (2 * a);
    float tMax = (-b + sqrt_b2p4ac) / (2 * a);

    if (tMin >= 0) return tMin;
    else
    { // tMin < 0
        if (tMax < 0) return defaultRet;
        else
        { // tMax >= 0
            return 0; // 已经相交了
        }
    }

    return defaultRet;
}

static float testCollide_Circle_Line(Primitive *p1, Primitive *p2)
{
    assert(p1->getType() == PT_Circle && p2->getType() == PT_Line);

    float defaultRet = -std::numeric_limits<float>::max();

    Circle *circle = static_cast<Circle*>(p1);
    Line *line = static_cast<Line*>(p2);

    core::vector2df verDir(line->getDir().Y, -line->getDir().X); // 垂线
    float pr = circle->getVec().dotProduct(verDir);
    if (core::equals(pr, 0.0f)) return defaultRet; // 平行
    
    float pl = (line->getPoint() - circle->getPos()).dotProduct(verDir);
    float k = pl / pr;  // 交点, 对应的球移动时间
    
    float circleSpeed = circle->getVec().getLength();
    assert(circleSpeed > 0); // 速率不应该为0
    core::vector2df circleVecDir = core::vector2df(circle->getVec()).normalize();
    float cosAngle = line->getDir().dotProduct(circleVecDir);
    if (core::equals(fabs(cosAngle), 1.0f)) return defaultRet; // 平行
    assert(sinf(acosf(cosAngle)) > 0); // 由不平行保证
    float bias = fabs(circle->getRadius() / sinf(acosf(cosAngle)) / circleSpeed); // 球和线相交的偏移, 对应的球移动时间
    
    float minK = k - bias, maxK = k + bias;
    if (minK >= 0) return minK;
    else
    { // minK < 0
        if (maxK < 0) return defaultRet;
        else
        {   // maxK >= 0
            return 0; // 已经相交了
        }
    }

    return defaultRet;
}

typedef float (*FuncT_TestCollide)(Primitive *, Primitive *);
////////////////////////////////////////////////////////////////////////////
static void collide_Circle_Circle(Primitive *p1, Primitive *p2)
{
    assert(p1->getType() == PT_Circle && p2->getType() == PT_Circle);

    Physics::Circle *circle0 = static_cast<Physics::Circle*>(p1);
    Physics::Circle *circle1 = static_cast<Physics::Circle*>(p2);

    core::vector2df collideDir = (circle0->getPos() - circle1->getPos()).normalize();

    core::vector2df v0x = circle0->getVec().dotProduct(collideDir) * collideDir;
    core::vector2df v0y = circle0->getVec() - v0x;
    core::vector2df v1x = circle1->getVec().dotProduct(collideDir) * collideDir;
    core::vector2df v1y = circle1->getVec() - v1x;

    core::vector2df _v0x = (circle0->getMass() - circle1->getMass()) * v0x + 2 * circle1->getMass() * v1x;
    _v0x /= (circle0->getMass() + circle1->getMass());
    core::vector2df _v1x = (circle1->getMass() - circle0->getMass()) * v1x + 2 * circle0->getMass() * v0x;
    _v1x /= (circle0->getMass() + circle1->getMass());

    circle0->setVec(_v0x + v0y);
    circle1->setVec(_v1x + v1y);
}

static void collide_Circle_Line(Primitive *p1, Primitive *p2)
{
    assert(p1->getType() == PT_Circle && p2->getType() == PT_Line);

    Circle *circle = static_cast<Circle*>(p1);
    Line *line = static_cast<Line*>(p2);

    core::vector2df x = circle->getVec().dotProduct(line->getDir()) * line->getDir();
    core::vector2df y = circle->getVec() - x;

    circle->setVec(x - y);
}

typedef void (*FuncT_Collide)(Primitive *, Primitive *);
////////////////////////////////////////////////////////////////////////////
class CollideServer
{
public:
    void addTestCollideFunc(PrimitiveType t1, PrimitiveType t2, FuncT_TestCollide cf);
    void addCollideFunc(PrimitiveType t1, PrimitiveType t2, FuncT_Collide cf);
    float testCollide(Primitive *p1, Primitive *p2);
    void collide(Primitive *p1, Primitive *p2);

private:
    static s32 makeKey(PrimitiveType t1, PrimitiveType t2);
    
private:
    typedef core::map<s32, FuncT_TestCollide>   TestCollideFuncMap;
    typedef core::map<s32, FuncT_Collide>       CollideFuncMap;

private:
    TestCollideFuncMap      m_testFuncMap;
    CollideFuncMap          m_funcMap;
};

s32 CollideServer::makeKey(PrimitiveType t1, PrimitiveType t2)
{
    assert(t1 <= t2 && t1 <= 0xffff && t2 <= 0xffff);
    return (t1 << 16) | t2;
}

void CollideServer::addTestCollideFunc(PrimitiveType t1, PrimitiveType t2, FuncT_TestCollide cf)
{
    assert(t1 <= t2);
    assert(m_testFuncMap.find(makeKey(t1, t2)) == NULL);
    m_testFuncMap[makeKey(t1, t2)] = cf;
}   

float CollideServer::testCollide(Primitive *p1, Primitive *p2)
{
    if (p2->getType() < p1->getType()) core::swap(p1, p2);
    TestCollideFuncMap::Node *node = m_testFuncMap.find(makeKey(p1->getType(), p2->getType()));
    if (node == NULL) return -1;
    return node->getValue()(p1, p2);
}

void CollideServer::addCollideFunc(PrimitiveType t1, PrimitiveType t2, FuncT_Collide cf)
{
    assert(t1 <= t2);
    assert(m_funcMap.find(makeKey(t1, t2)) == NULL);
    m_funcMap[makeKey(t1, t2)] = cf;
}

void CollideServer::collide(Primitive *p1, Primitive *p2)
{
    if (p2->getType() < p1->getType()) core::swap(p1, p2);
    FuncT_Collide func = m_funcMap[makeKey(p1->getType(), p2->getType())];
    assert(func != NULL);
    func(p1, p2);
}
////////////////////////////////////////////////////////////////////////////
class World:
    public IWorld
{
public:
    World();
    ~World();

protected:
    virtual void update(u32 timeMs);
    virtual void pause();
    virtual void setMotionFriction(float fricion);

    virtual void addListener(IWorldListener *listener);
    virtual void removeListener(IWorldListener *listener);
    virtual void removeAllListener();

    virtual Primitive* addPrimitive(const core::stringc& name, Primitive* p);
    virtual void removePrimitive(const core::stringc& name);
    virtual void removeAllPrimitive();
    virtual Primitive* getPrimitive(const core::stringc& name);

private:
    void notify_addPrimitive(IWorldListener *listener, Primitive *p);
    void notify_removePrimitive(IWorldListener *listener, Primitive *p);
    void notify_prePrimitiveCollide(Primitive *p1, Primitive *p2);
    void notify_postPrimitiveCollide(Primitive *p1, Primitive *p2);
    void getPrimitiveArray(core::array<Primitive*> *array);

private:
    typedef core::map<core::stringc, Primitive*>    PrimitiveMap;
    

private:
    core::array<IWorldListener*>    m_listeners;
    PrimitiveMap                    m_primitives;
    CollideServer                   m_collideServer;
    u32                             m_lastTimeMs;
    float                           m_friction;
    bool                            m_pause;
};

World::World(void):
m_lastTimeMs(0), m_friction(0), m_pause(false)
{
    m_collideServer.addTestCollideFunc(Circle::_getType(), Circle::_getType(), testCollide_Circle_Circle);
    m_collideServer.addTestCollideFunc(Circle::_getType(), Line::_getType(), testCollide_Circle_Line);
    m_collideServer.addCollideFunc(Circle::_getType(), Circle::_getType(), collide_Circle_Circle);
    m_collideServer.addCollideFunc(Circle::_getType(), Line::_getType(), collide_Circle_Line);
}

World::~World(void)
{
    removeAllListener();
    removeAllPrimitive();
}

void World::update(u32 timeMs)
{
    if (m_lastTimeMs == 0) m_lastTimeMs = timeMs;

    float elapse = (timeMs - m_lastTimeMs) / 1000.0f;
    m_lastTimeMs = timeMs;

    if (m_pause) return;

    core::array<Primitive*> primitives;
    getPrimitiveArray(&primitives);

    while (elapse > 0)
    {
        float subElapse = elapse; // 这个是会发生下次碰撞的最早时间(也或者是结束时间)

        core::array<u32> iCollideAftMove, jCollideAftMove;
        core::array<u32> iCollideAlways, jCollideAlways;

        for (u32 i = 0; i < primitives.size(); ++i)
        {
            for (u32 j = i + 1; j < primitives.size(); ++j)
            {
                float collideDis = m_collideServer.testCollide(primitives[i], primitives[j]);
                if (collideDis > std::numeric_limits<float>::epsilon()) // 这个判断保证每次向前更新
                {
                    if (collideDis < subElapse)  // 找一个最小步进
                    { 
                        subElapse = collideDis;
                        iCollideAftMove.clear(), jCollideAftMove.clear();
                        iCollideAftMove.push_back(i), jCollideAftMove.push_back(j);
                    }
                    else if (collideDis == subElapse)
                    {
                        iCollideAftMove.push_back(i), jCollideAftMove.push_back(j);
                    }
                    else {}  // 碰撞太晚
                }
                else if (collideDis > 0 || core::equals(collideDis, 0.0f))
                {
                    iCollideAlways.push_back(i), jCollideAlways.push_back(j);
                }
                else {} // 不碰撞
            }
        }

        for (u32 i = 0; i < primitives.size(); ++i)
        {
            primitives[i]->update(subElapse);
        }

        for (u32 k = 0; k < iCollideAftMove.size(); ++k)
        { // 移动后才会碰撞的
            u32 i = iCollideAftMove[k], j = jCollideAftMove[k];
            notify_prePrimitiveCollide(primitives[i], primitives[j]);
            m_collideServer.collide(primitives[i], primitives[j]);
            notify_postPrimitiveCollide(primitives[i], primitives[j]);
        }
        for (u32 k = 0; k < iCollideAlways.size(); ++k)
        { // 移动前就会碰撞的
            u32 i = iCollideAlways[k], j = jCollideAlways[k];
            notify_prePrimitiveCollide(primitives[i], primitives[j]);
            m_collideServer.collide(primitives[i], primitives[j]); // 重要!如果没有这句可能发生重叠
            notify_postPrimitiveCollide(primitives[i], primitives[j]);
        }

        if (m_friction > 0)
        { // 滑动摩擦
            for (u32 i = 0; i < primitives.size(); ++i)
            {
                primitives[i]->applyMotionFriction(m_friction, subElapse);
            }
        }

        elapse -= subElapse;
    }
}

void World::pause()
{
    m_pause = !m_pause;
}

void World::setMotionFriction(float fricion)
{
    m_friction = fricion;
}

void World::getPrimitiveArray(core::array<Primitive*> *array)
{
    assert(array->empty());
    PrimitiveMap::Iterator iter = m_primitives.getIterator();
    while (!iter.atEnd())
    {
        array->push_back(iter.getNode()->getValue());
        iter++;
    }
}

void World::addListener(IWorldListener *listener)
{
    assert(m_listeners.linear_search(listener) == -1);
    m_listeners.push_back(listener);
    notify_addPrimitive(listener, NULL);
}

void World::removeListener(IWorldListener *listener)
{
    s32 pos = m_listeners.linear_search(listener);
    if (pos != -1) m_listeners.erase(pos);
    notify_removePrimitive(listener, NULL);
}

void World::removeAllListener()
{
    while (!m_listeners.empty())
    {
        removeListener(m_listeners[0]);
    }
}

Primitive* World::addPrimitive(const core::stringc& name, Primitive* p)
{
    assert(getPrimitive(name) == NULL);
    p->grab();
    m_primitives[name] = p;
    notify_addPrimitive(NULL, p);
    return p;
}

void World::removePrimitive(const core::stringc& name)
{
    if (Primitive *p = getPrimitive(name))
    {
        notify_removePrimitive(NULL, p);
        p->drop();
    }
}

void World::removeAllPrimitive()
{
    PrimitiveMap::Iterator iter = m_primitives.getIterator();
    while (!iter.atEnd())
    {
        Primitive *p = iter.getNode()->getValue();
        iter++;
        notify_removePrimitive(NULL, p);
        p->drop();
    }
    m_primitives.clear();
}

Primitive* World::getPrimitive(const core::stringc& name)
{
    if (PrimitiveMap::Node *node = m_primitives.find(name))
    {
        return node->getValue();
    }
    return NULL;
}

void World::notify_addPrimitive(IWorldListener *listener, Primitive *p)
{
    core::array<IWorldListener*> listeners;
    if (listener == NULL) listeners = m_listeners;
    else listeners.push_back(listener);

    core::array<Primitive*> primitves;
    if (p == NULL) 
    {
        getPrimitiveArray(&primitves);
    }
    else primitves.push_back(p);

    for (u32 i = 0; i < listeners.size(); ++i)
    {
        for (u32 j = 0; j < primitves.size(); ++j)
        {
            listeners[i]->onAddPhysicsPrimitive(primitves[j]);
        }
    }
}

void World::notify_removePrimitive(IWorldListener *listener, Primitive *p)
{
    core::array<IWorldListener*> listeners;
    if (listener == NULL) listeners = m_listeners;
    else listeners.push_back(listener);

    core::array<Primitive*> primitves;
    if (p == NULL) 
    {
        getPrimitiveArray(&primitves);
    }
    else primitves.push_back(p);

    for (u32 i = 0; i < listeners.size(); ++i)
    {
        for (u32 j = 0; j < primitves.size(); ++j)
        {
            listeners[i]->onRemovePhysicsPrimitive(primitves[j]);
        }
    }
}

void World::notify_prePrimitiveCollide(Primitive *p1, Primitive *p2)
{
    core::array<IWorldListener*> listeners = m_listeners;
    for (u32 i = 0; i < listeners.size(); ++i)
    {
        listeners[i]->onPrePhysicsPrimitiveCollide(p1, p2);
    }
}

void World::notify_postPrimitiveCollide(Primitive *p1, Primitive *p2)
{
    core::array<IWorldListener*> listeners = m_listeners;
    for (u32 i = 0; i < listeners.size(); ++i)
    {
        listeners[i]->onPostPhysicsPrimitiveCollide(p1, p2);
    }
}

///////////////////////////////////////////////////////////////////////////

static World* g_physicsWorld = NULL;
IWorld* IWorld::createSingleton()
{
    assert(g_physicsWorld == NULL);
    return g_physicsWorld = new World();
}

void IWorld::destroySingleton()
{
    assert(g_physicsWorld != NULL);
    delete g_physicsWorld;
    g_physicsWorld = NULL;
}

IWorld* IWorld::getSingletonPtr()
{
    assert(g_physicsWorld != NULL);
    return g_physicsWorld;
}

}