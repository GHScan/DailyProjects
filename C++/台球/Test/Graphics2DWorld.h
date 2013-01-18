#pragma once

#include "GraphicsWorld.h"

namespace Graphics
{

class World2D:
    public IWorld
{
public:
    World2D(IrrlichtDevice *device);
    ~World2D();

protected:
    virtual const core::stringc getType() const;
    virtual void update(u32 timeMs);

protected:
    virtual void onAddPhysicsPrimitive(Physics::Primitive *p);
    virtual void onRemovePhysicsPrimitive(Physics::Primitive *p);
    
private:
    s32 inverseY(s32 y);

private:
    typedef core::array<Physics::Primitive*> PrimitiveList;

private:
    PrimitiveList    m_primitiveList;
    IrrlichtDevice  *m_device;
};


}