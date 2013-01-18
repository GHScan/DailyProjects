#pragma once

#include "GraphicsWorld.h"

namespace Graphics
{

class World3D:
    public IWorld
{
public:
    World3D(IrrlichtDevice *device);
    ~World3D();

protected:
    virtual const core::stringc getType() const;
    virtual void update(u32 timeMs);

protected:
    virtual void onAddPhysicsPrimitive(Physics::Primitive *p);
    virtual void onRemovePhysicsPrimitive(Physics::Primitive *p);

private:
    typedef core::map<Physics::Primitive*, scene::ISceneNode*>  PrimitiveMap;

private:
    PrimitiveMap    m_primitiveMap;
    IrrlichtDevice *m_device;
};

}