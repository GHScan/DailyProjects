#include "StdAfx.h"

#include "Graphics3DWorld.h"

namespace Graphics
{
World3D::World3D(IrrlichtDevice *device):
m_device(device)
{
    scene::ISceneManager *mgr = m_device->getSceneManager();

    core::dimension2du screenSize = mgr->getVideoDriver()->getScreenSize();
    scene::ICameraSceneNode *camera = mgr->addCameraSceneNode(
        NULL, core::vector3df(screenSize.Width / 2.0f, 300, -300), core::vector3df(screenSize.Width / 2.0f, 0, 0));
    camera->setUpVector(core::vector3df(0, 0, 1));

    scene::ILightSceneNode *light = mgr->addLightSceneNode();
    video::SLight l;
    l.Type = video::ELT_DIRECTIONAL;
    l.AmbientColor = video::SColorf(0.1f, 0, 0);
    l.DiffuseColor = video::SColorf(0.8f, 0, 0);
    l.SpecularColor = video::SColorf(1.0f,1.0f, 1.0f);
    light->setLightData(l);
    light->setName("world3dLight");
    light->setRotation(core::vector3df(45.0f, -45.0f, 0));

    {
        // Ä§Êý!!!!~~~~~~~~~~~~~~
        scene::IAnimatedMesh *mesh = mgr->getMesh("media_billiard/Ì¨Çò×À.obj");
        scene::IMeshSceneNode *node = mgr->addMeshSceneNode(mesh);
        node->setName("billiardTable");
        node->setPosition(core::vector3df(300, -300, 300));
        node->setScale(core::vector3df(8, 6, 8));
    }
}

World3D::~World3D(void)
{
    scene::ISceneNode *node = NULL;

    node = m_device->getSceneManager()->getSceneNodeFromName("billiardTable");
    node->getParent()->removeChild(node);

    node = m_device->getSceneManager()->getSceneNodeFromName("world3dLight");
    node->getParent()->removeChild(node);
    m_device->getSceneManager()->setActiveCamera(NULL);
}

void World3D::update(u32 timeMs)
{
    PrimitiveMap::Iterator iter = m_primitiveMap.getIterator();
    while (!iter.atEnd())
    {
        PrimitiveMap::Node *node = iter.getNode();

        Physics::Primitive *p = node->getKey();
        switch (p->getType())
        {
        case Physics::PT_Circle:
            {
                Physics::Circle *circle = static_cast<Physics::Circle*>(p);
                core::vector2df pos = circle->getPos();
                node->getValue()->setPosition(core::vector3df(pos.X, 0, pos.Y));
            }
            break;
        default:
            break;
        }

        iter++;
    }
}

const core::stringc World3D::getType() const { return "3d"; }


void World3D::onAddPhysicsPrimitive(Physics::Primitive *p)
{
    assert(m_primitiveMap.find(p) == NULL);
    switch (p->getType())
    {
    case Physics::PT_Circle:
        {
            p->grab();
            scene::ISceneNode *node = 
            m_device->getSceneManager()->addSphereSceneNode(
                static_cast<Physics::Circle*>(p)->getRadius(), 32);
            m_primitiveMap[p] = node;

            video::SMaterial& mat = node->getMaterial(0);
            mat.SpecularColor = video::SColor(196, 196, 196, 196);
            mat.Shininess = 8;
        }
        break;
    default:
        break;
    }
}

void World3D::onRemovePhysicsPrimitive(Physics::Primitive *p)
{
    PrimitiveMap::Node * mapNode = m_primitiveMap.find(p);
    if (mapNode != NULL)
    {
        p->drop();
        scene::ISceneNode *node = mapNode->getValue();
        node->getParent()->removeChild(node);
        
        m_primitiveMap.remove(p);
    }
}

}