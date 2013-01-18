#include "StdAfx.h"

#include <irrKlang.h>

#include "SoundWorld.h"

namespace Sound
{
///////////////////////////////////////////////////////////////////////////
class CollideSoundServer
{
public:
    CollideSoundServer();
    ~CollideSoundServer();
    void addSoundFile(Physics::PrimitiveType t1, Physics::PrimitiveType t2, const core::stringc& file);
    void playSound(Physics::PrimitiveType t1, Physics::PrimitiveType t2);

private:
    static u32 makeKey(Physics::PrimitiveType t1, Physics::PrimitiveType t2);

private:
    typedef core::map<u32, core::stringc>   SoundMap;

private:
    irrklang::ISoundEngine *m_soundEngine;
    SoundMap                m_collideSoundMap;
};

CollideSoundServer::CollideSoundServer()
{
    m_soundEngine = irrklang::createIrrKlangDevice(); // 多线程的
}

CollideSoundServer::~CollideSoundServer()
{   
    m_soundEngine->drop();
    m_soundEngine = NULL;
}

void CollideSoundServer::addSoundFile(Physics::PrimitiveType t1, Physics::PrimitiveType t2, const core::stringc& file)
{
    assert(m_collideSoundMap.find(makeKey(t1, t2)) == NULL);
    m_collideSoundMap[makeKey(t1, t2)] = file;
}

void CollideSoundServer::playSound(Physics::PrimitiveType t1, Physics::PrimitiveType t2)
{
    SoundMap::Node *node = m_collideSoundMap.find(makeKey(t1, t2));
    if (node != NULL) 
    { 
        void* p = m_soundEngine->play2D(node->getValue().c_str());
        assert(p == NULL);
    }
}


u32 CollideSoundServer::makeKey(Physics::PrimitiveType t1, Physics::PrimitiveType t2)
{
    assert(t1 <= t2 && t2 <= 0xffff);
    return (t1 << 16) | t2;
}
///////////////////////////////////////////////////////////////////////////
class World:
    public IWorld
{
public:
    World();
    ~World();

protected:
    virtual void onPrePhysicsPrimitiveCollide(Physics::Primitive *p1, Physics::Primitive *p2);

private:
    void addCollideSoundFile(Physics::PrimitiveType t1, Physics::PrimitiveType t2, const core::stringc& file);
    void playCollideSound(Physics::PrimitiveType t1, Physics::PrimitiveType t2);
    u32 makeKey();

private:
    CollideSoundServer  m_soundServer;
};

World::World()
{
    m_soundServer.addSoundFile(Physics::Circle::_getType(), Physics::Circle::_getType(),
        "media_billiard/碰球.wav");
    m_soundServer.addSoundFile(Physics::Circle::_getType(), Physics::Line::_getType(),
        "media_billiard/碰边.wav");
}

World::~World()
{
}

void World::onPrePhysicsPrimitiveCollide(Physics::Primitive *p1, Physics::Primitive *p2)
{
    if (p2->getType() < p1->getType()) core::swap(p1, p2);
    m_soundServer.playSound(p1->getType(), p2->getType());
}


///////////////////////////////////////////////////////////////////////////

IWorld *g_soundWolrd = NULL;
IWorld* IWorld::createSingleton()
{
    assert(g_soundWolrd == NULL);
    return g_soundWolrd = new World();
}

void IWorld::destroySingleton()
{
    assert(g_soundWolrd != NULL);
    delete g_soundWolrd;
    g_soundWolrd = NULL;
}

IWorld* IWorld::getSingletonPtr()
{
    assert(g_soundWolrd != NULL);
    return g_soundWolrd;
}

}