#pragma once

#include "PhysicsWorld.h"

namespace irr
{

class IrrlichtDevice;

}

namespace Graphics
{

struct IWorld:
    public Physics::IWorldListener
{
    virtual ~IWorld() = 0 {}

    static IWorld* createSingleton(const core::stringc& type, IrrlichtDevice *device);
    static void destroySingleton();
    static IWorld* getSingletonPtr();

    virtual void update(u32 timeMs) = 0;
    virtual const core::stringc getType() const = 0;
};


}