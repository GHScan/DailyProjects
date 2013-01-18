#pragma once

namespace Physics
{

enum PrimitiveType
{
    PT_Circle = 1,
    PT_Line,
};

class Primitive:
    public IReferenceCounted
{
public:
    virtual PrimitiveType getType() const = 0;
    virtual void update(float elapse){}
    virtual void applyMotionFriction(float fricion, float elapse){}
};

class Line:
    public Primitive
{
public:
    Line(const core::vector2df& dir, const core::vector2df& pt, bool normalizeDir = true);
    const core::vector2df& getDir() const;
    const core::vector2df& getPoint() const;

    static PrimitiveType _getType();

protected:
    virtual PrimitiveType getType() const;

private:
    core::vector2df     m_dir;
    core::vector2df     m_pt;
};

class Circle:
    public Primitive
{
public:
    Circle(float radius, float mass, 
        const core::vector2df& pos = core::vector2df(),
        const core::vector2df& vec = core::vector2df());
    float getRadius() const;
    float getMass() const;
    const core::vector2df& getPos() const;
    const core::vector2df& getVec() const;
    void setPos(const core::vector2df& pos);
    void setVec(const core::vector2df& vec);

    static PrimitiveType _getType();

protected:
    virtual PrimitiveType getType() const;
    virtual void update(float elapse);
    virtual void applyMotionFriction(float fricion, float elapse);

private:
    float           m_radius;
    float           m_mass;
    core::vector2df m_pos;
    core::vector2df m_vec;
};

struct IWorldListener
{
    virtual ~IWorldListener() = 0 {}

    virtual void onAddPhysicsPrimitive(Primitive *p){}
    virtual void onRemovePhysicsPrimitive(Primitive *p){}
    virtual void onPrePhysicsPrimitiveCollide(Primitive *p1, Primitive *p2){}
    virtual void onPostPhysicsPrimitiveCollide(Primitive *p1, Primitive *p2){}
};

struct IWorld
{
    static IWorld* createSingleton();
    static void destroySingleton();
    static IWorld* getSingletonPtr();

    virtual void update(u32 timeMs) = 0;
    virtual void pause() = 0;
    virtual void setMotionFriction(float fricion) = 0;

    virtual void addListener(IWorldListener *listener) = 0;
    virtual void removeListener(IWorldListener *listener) = 0;
    virtual void removeAllListener() = 0;

    virtual Primitive* addPrimitive(const core::stringc& name, Primitive* p) = 0;
    virtual void removePrimitive(const core::stringc& name) = 0;
    virtual void removeAllPrimitive() = 0;
    virtual Primitive* getPrimitive(const core::stringc& name) = 0;
};


}