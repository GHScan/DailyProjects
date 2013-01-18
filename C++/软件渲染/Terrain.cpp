// vim: fileencoding=gbk
#include "pch.h"

#include <cassert>

#include "Terrain.h"

FlatTerrain::FlatTerrain(float y):
    m_y(y)
{
}
float FlatTerrain::getHeight(float x, float z) const
{
    return m_y;
}

MeshTrrain::MeshTrrain()
{
}
MeshTrrain::MeshTrrain(const std::string& fname)
{
}
bool MeshTrrain::load(const std::string& fname)
{
    return false;
}
void MeshTrrain::save(const std::string& fname) const
{
}

float MeshTrrain::getHeight(float x, float z) const
{
    assert(0);
    return 0;
}
