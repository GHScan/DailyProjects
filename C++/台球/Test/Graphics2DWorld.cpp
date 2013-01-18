#include "StdAfx.h"

#include "Graphics2DWorld.h"

namespace Graphics
{
////////////////////////////////////////////////////////////////////////////
const float LINE_LEN = 1000.0f;

World2D::World2D(IrrlichtDevice *device):
m_device(device)
{
    assert(m_device != NULL);
}

World2D::~World2D()
{
}

const core::stringc World2D::getType() const { return "2d"; }

s32 World2D::inverseY(s32 y)
{
    return (s32)m_device->getVideoDriver()->getScreenSize().Height - y;
}

void World2D::update(u32 timeMs)
{
    video::IVideoDriver *driver = m_device->getVideoDriver();
    video::SColor color(255, 255, 0, 0);

    for (u32 i = 0; i < m_primitiveList.size(); ++i)
    {
        Physics::Primitive *p = m_primitiveList[i];
        switch (p->getType())
        {
        case Physics::PT_Circle:
            {
                Physics::Circle *circle = static_cast<Physics::Circle*>(p);
                driver->draw2DPolygon(
                    core::vector2di((s32)circle->getPos().X, inverseY((s32)circle->getPos().Y)), 
                    circle->getRadius(),
                    color, 30);
            }
            break;
        case Physics::PT_Line:
            {
                Physics::Line *line = static_cast<Physics::Line*>(p);
                core::vector2df start(line->getPoint() - LINE_LEN * line->getDir());
                core::vector2df end(line->getPoint() + LINE_LEN * line->getDir());
                driver->draw2DLine(
                    core::vector2di((s32)start.X, inverseY((s32)start.Y)), 
                    core::vector2di((s32)end.X, inverseY((s32)end.Y)),
                    color);
            }
            break;
        default:
            assert(0);
            break;
        }
    }
}

void World2D::onAddPhysicsPrimitive(Physics::Primitive *p)
{
    assert(m_primitiveList.linear_search(p) == -1);
    m_primitiveList.push_back(p);
}

void World2D::onRemovePhysicsPrimitive(Physics::Primitive *p)
{
    s32 pos = m_primitiveList.linear_search(p);
    if (pos != -1) m_primitiveList.erase(pos);
}
////////////////////////////////////////////////////////////////////////////

}