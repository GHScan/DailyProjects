// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>
#include <cmath>

#include <algorithm>

#include "EntityController.h"
#include "VirtualPlatform.h"
#include "SceneManager.h"
#include "Camera.h"
#include "Util.h"
#include "Entity.h"
#include "Vector.h"
#include "Traceable.h"

EntityController_Rotator::EntityController_Rotator(SceneManager *sceneMgr):
    m_sceneMgr(sceneMgr), m_pickedEntity(NULL), m_oldEntWorldMat(Matrix4x4::IDENTITY),
    m_oldX(0), m_oldY(0)
{
}

void EntityController_Rotator::onMouseButtonDown(int btn, float x, float y)
{
    if (btn == MOUSE_LBUTTON) {
        const Camera *camera = m_sceneMgr->getCamera();
        Ray r = camera->getRayFromViewport(x, y);
        TraceFragment frag;
        m_pickedEntity = m_sceneMgr->pickClosestEntity(r, true, frag);
        if (m_pickedEntity == NULL) return;
        m_oldX = x, m_oldY = y;
        m_oldEntWorldMat = m_pickedEntity->getSceneNode()->getWorldMatrix();
    }
}
void EntityController_Rotator::onMouseButtonUp(int btn, float x, float y)
{
    if (btn == MOUSE_LBUTTON) {
        m_pickedEntity = NULL;
    }
}

static Vector3 screenPoint2SpherePoint(float x, float y)
{
    x = std::max(0.f, std::min(x, 1.f));
    y = std::max(0.f, std::min(y, 1.f));
    Vector2 v2(x * 2 - 1, 1 - y * 2);
    Vector3 v3;
    if (v2.lengthSqr() >= 1)  {
        v3 = Vector3(v2.normalize(), 0);
    }
    else {
        v3 = Vector3(v2.x, v2.y, -sqrt(1 - v2.lengthSqr()));
    }
    return v3;
}
void EntityController_Rotator::onMouseMove(float x, float y)
{
    if (m_pickedEntity != NULL) {
        Vector3 startVec = screenPoint2SpherePoint(m_oldX, m_oldY);
        Vector3 endVec = screenPoint2SpherePoint(x, y);
        assert(startVec.isUnit());
        assert(endVec.isUnit());
        Vector3 axis = startVec.crossProduct(endVec);
        float angle = radian2Degree(asin(axis.length()));
        axis = axis.normalize();

        const Camera *camera = m_sceneMgr->getCamera();
        Matrix4x4 viewMat = camera->getViewMatrix();
        Matrix4x4 mat = m_oldEntWorldMat * viewMat;
        Vector4 trans(mat[3]);
        mat *= 
            Matrix4x4::fromTranslate(-trans.x, -trans.y, -trans.z) *
            Matrix4x4::fromRotateAxis(axis, angle) *
            Matrix4x4::fromTranslate(trans.x, trans.y, trans.z) * 
            camera->getSceneNode()->getWorldMatrix();
        m_pickedEntity->getSceneNode()->setWorldMatrix(mat);

        m_sceneMgr->notifyCameraSpaceChanged();
    }
}
