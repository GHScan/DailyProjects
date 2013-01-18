// vim: fileencoding=gbk

#ifndef ENTITYCONTROLLER_H
#define ENTITYCONTROLLER_H

#include "Matrix.h"

class SceneManager;
class Entity;

struct IEntityController
{
    virtual ~IEntityController() = 0 {}
    virtual void onMouseButtonDown(int btn, float x, float y) = 0;
    virtual void onMouseButtonUp(int btn, float x, float y) = 0;
    virtual void onMouseMove(float x, float y) = 0;
};

class EntityController_Rotator:
    public IEntityController
{
public:
    EntityController_Rotator(SceneManager *sceneMgr);

    virtual void onMouseButtonDown(int btn, float x, float y);
    virtual void onMouseButtonUp(int btn, float x, float y);
    virtual void onMouseMove(float x, float y);
private:
    SceneManager *m_sceneMgr;
    Entity *m_pickedEntity;
    Matrix4x4 m_oldEntWorldMat;
    float m_oldX, m_oldY;
};

#endif // #ifndef ENTITYCONTROLLER_H
