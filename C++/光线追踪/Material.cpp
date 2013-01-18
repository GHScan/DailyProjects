// vim: fileencoding=gbk
#include "pch.h"

#include "Material.h"
#include "Serialize.h"
#include "Util.h"

std::ostream& operator << (std::ostream& so, const Material& m)
{
    StreamBlockWriter w("Material", so);
    w.write("ambient", m.ambientClr.data(), m.ambientClr.data() + 3);
    w.write("diffuse", m.diffuseClr.data(), m.diffuseClr.data() + 3);
    w.write("specular", m.specularClr.data(), m.specularClr.data() + 3);
    w.write("power", m.power);
    w.write("lightingLevel", m.lightingLevel);

    w.write("texture", m.texture.c_str());
    w.write("bumpTexture", m.bumpTexture.c_str());
    w.write("texFilter", enum2Str(m.texFilter));
    w.write("texAddressMode", enum2Str(m.texAddressMode));

    w.write("reflectible", (int)m.reflectible);
    w.write("refractable", (int)m.refractable);
    w.write("refractIndex", m.refractIndex);
    w.write("reflectAndRefractWeight", m.reflectAndRefractWeight);
    w.write("fresnelBias", m.fresnelBias);
    w.write("fresnelPower", m.fresnelPower);

    return so;
}

std::istream& operator >> (std::istream& si, Material& m)
{
    StreamBlockReader r("Material", si);

    if (!r.read("ambient", m.ambientClr.data(), m.ambientClr.data() + 3)) assert(0);
    if (!r.read("diffuse", m.diffuseClr.data(), m.diffuseClr.data() + 3)) assert(0);
    if (!r.read("specular", m.specularClr.data(), m.specularClr.data() + 3)) assert(0);
    if (!r.read("power", &m.power)) assert(0);
    if (!r.read("lightingLevel", &m.lightingLevel)) assert(0);
    assert(m.lightingLevel >= 0 && m.lightingLevel <= 3);

    if (!r.read("texture", m.texture)) assert(0);
    if (!r.read("bumpTexture", m.bumpTexture)) assert(0);
    {
        std::string s;
        if (!r.read("texFilter", s)) assert(0);
        str2Enum(m.texFilter, s.c_str());
        if (!r.read("texAddressMode", s)) assert(0);
        str2Enum(m.texAddressMode, s.c_str());
    }

    {
        int t = 0;
        if (!r.read("reflectible", &t)) assert(0);
        m.reflectible = t != 0;
        if (!r.read("refractable", &t)) assert(0);
        m.refractable = t != 0;
    }
    if (!r.read("refractIndex", &m.refractIndex)) assert(0);
    if (!r.read("reflectAndRefractWeight", &m.reflectAndRefractWeight)) assert(0);
    if (!r.read("fresnelBias", &m.fresnelBias)) assert(0);
    if (!r.read("fresnelPower", &m.fresnelPower)) assert(0);

    assert(!fequal(m.refractIndex, 0));
    m._refractInvIndex = 1 / m.refractIndex;

    return si;
}
