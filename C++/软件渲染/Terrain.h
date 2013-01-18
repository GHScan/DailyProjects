// vim: fileencoding=gbk

#ifndef TERRAIN_H
#define TERRAIN_H

#include <string>

struct ITerrain
{
    virtual ~ITerrain() = 0 {}
    virtual float getHeight(float x, float z) const = 0;
};

class FlatTerrain:
    public ITerrain
{
public:
    FlatTerrain(float y);
    virtual float getHeight(float x, float z) const;
private:
    float m_y;
};

class MeshTrrain:
    public ITerrain
{
public:
    MeshTrrain();
    MeshTrrain(const std::string& fname);
    bool load(const std::string& fname);
    void save(const std::string& fname) const;

    virtual float getHeight(float x, float z) const;
private:
};

#endif // #ifndef TERRAIN_H
