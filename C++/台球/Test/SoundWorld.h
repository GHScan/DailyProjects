#pragma once

#include "PhysicsWorld.h"

namespace Sound
{

struct IWorld:
    public Physics::IWorldListener
{
    static IWorld* createSingleton();
    static void destroySingleton();
    static IWorld* getSingletonPtr();
};

}
