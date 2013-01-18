#include "stdafx.h"

#include "GraphicsWorld.h"

#include "Graphics2DWorld.h"
#include "Graphics3DWorld.h"

namespace Graphics
{

static IWorld *g_graphicsWorld;
IWorld* IWorld::createSingleton(const core::stringc& type, IrrlichtDevice *device)
{
    assert(g_graphicsWorld == NULL);
    if (type.equals_ignore_case("2d"))
    {
        g_graphicsWorld = new World2D(device);
    }
    else if (type.equals_ignore_case("3d"))
    {
        g_graphicsWorld = new World3D(device);
    }
    else assert(0);

    assert(type.equals_ignore_case(g_graphicsWorld->getType()));

    return g_graphicsWorld;
}

void IWorld::destroySingleton()
{
    assert(g_graphicsWorld != NULL);
    delete g_graphicsWorld;
    g_graphicsWorld = NULL;
}

IWorld* IWorld::getSingletonPtr() { return g_graphicsWorld; }

}