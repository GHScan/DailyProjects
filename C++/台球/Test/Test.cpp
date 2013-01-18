// Test.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <time.h>

#include "PhysicsWorld.h"
#include "GraphicsWorld.h"
#include "SoundWorld.h"

IrrlichtDevice  *g_device = NULL;
Physics::IWorld *g_physicsWorld = NULL;

void sample_buildBilliards()
{
    const int m = 30;
    const int r = 17;
    for (int i = 0; i < 4; ++i)
    {
        core::stringc name = "buildBilliard_a";
        name.append('0' + i);
        g_physicsWorld->addPrimitive(name, new Physics::Circle(r, m, core::vector2df(400, 100 + i * 40)))->drop();
    }
    for (int i = 0; i < 3; ++i)
    {
        core::stringc name = "buildBilliard_b";
        name.append('0' + i);
        g_physicsWorld->addPrimitive(name, new Physics::Circle(r, m, core::vector2df(400 - r * 1.732f, 120 + i * 40)))->drop();
    }
    for (int i = 0; i < 2; ++i)
    {
        core::stringc name = "buildBilliard_c";
        name.append('0' + i);
        g_physicsWorld->addPrimitive(name, new Physics::Circle(r, m, core::vector2df(400 - r * 1.732f * 2, 140 + i * 40)))->drop();
    }
    for (int i = 0; i < 1; ++i)
    {
        core::stringc name = "buildBilliard_d";
        name.append('0' + i);
        g_physicsWorld->addPrimitive(name, new Physics::Circle(r, m, core::vector2df(400 - r * 1.732f * 3, 160 + i * 40)))->drop();
    }
    const int v = 300;
    g_physicsWorld->addPrimitive("buildBilliard_e", 
        new Physics::Circle(r, m, 
        core::vector2df(100, 160), core::vector2df(v, rand() % 200 - 100)))->drop();
    g_physicsWorld->setMotionFriction(v * m / 28);
}

void sample_buildFrame()
{
    const int frameW = 1;
    g_physicsWorld->addPrimitive("frame_0", 
        new Physics::Line(core::vector2df(0, 1), 
        core::vector2df(frameW, frameW)))->drop();
    g_physicsWorld->addPrimitive("frame_2", 
        new Physics::Line(core::vector2df(0, 1), 
        core::vector2df(g_device->getVideoDriver()->getScreenSize().Width - frameW, frameW)))->drop();
    g_physicsWorld->addPrimitive("frame_1", 
        new Physics::Line(core::vector2df(1, 0), 
        core::vector2df(frameW, frameW)))->drop();
    g_physicsWorld->addPrimitive("frame_3", 
        new Physics::Line(core::vector2df(1, 0), 
        core::vector2df(frameW, g_device->getVideoDriver()->getScreenSize().Height - frameW)))->drop();
}

void build_samples()
{
    sample_buildFrame();
    sample_buildBilliards();
}

class EventListener:
    public IEventReceiver
{
    virtual bool OnEvent(const SEvent& evt)
    {
        switch (evt.EventType)
        {
        case EET_KEY_INPUT_EVENT:
            if (evt.KeyInput.PressedDown) break;
            switch (evt.KeyInput.Key)
            {
            case KEY_ESCAPE:
                g_device->closeDevice();
                return true;
            case KEY_KEY_2:
            case KEY_KEY_3:
                g_physicsWorld->removeListener(Graphics::IWorld::getSingletonPtr());
                Graphics::IWorld::destroySingleton();
                g_physicsWorld->addListener(
                Graphics::IWorld::createSingleton(
                    evt.KeyInput.Key == KEY_KEY_2 ? "2d" : "3d", g_device));
                return true;
            case KEY_KEY_P:
                g_physicsWorld->pause();
                break;
            case KEY_SPACE:
                g_physicsWorld->removeAllPrimitive();
                build_samples();
                break;
            default:
                break;
            }

            break;
        default:
            break;
        }
        return false;
    }
};

int main()
{
    srand((unsigned)time(NULL));

    EventListener eventReceiver;
    g_device = createDevice(
        video::EDT_DIRECT3D9, core::dimension2d<u32>(566, 310), 32, false, false, false, &eventReceiver);

    g_physicsWorld = Physics::IWorld::createSingleton();
    g_physicsWorld->addListener(Graphics::IWorld::createSingleton("3d", g_device));
    g_physicsWorld->addListener(Sound::IWorld::createSingleton());

    build_samples();

    video::IVideoDriver *d3d = g_device->getVideoDriver();
    scene::ISceneManager *sceneMgr = g_device->getSceneManager();
    while (g_device->run())
    {
        if (g_device->isWindowActive())
        {
            d3d->beginScene(true, true, video::SColor(255, 70, 163, 255));

            u32 nowTime = g_device->getTimer()->getTime();
            Physics::IWorld::getSingletonPtr()->update(nowTime);
            Graphics::IWorld::getSingletonPtr()->update(nowTime);

            sceneMgr->drawAll();
            d3d->endScene();
        }
        else Sleep(1);
    }

    
    g_physicsWorld->removeAllListener();

    Sound::IWorld::destroySingleton();
    Graphics::IWorld::destroySingleton();
    Physics::IWorld::destroySingleton();

    g_device->drop();
}