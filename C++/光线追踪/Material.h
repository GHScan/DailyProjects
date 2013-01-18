// vim: fileencoding=gbk

#ifndef MATERIAL_H
#define MATERIAL_H

#include "Vector.h"
#include "RenderStateEnums.h"

struct Material
{
    Vector3 ambientClr;
    Vector3 diffuseClr;
    Vector3 specularClr;
    float power;
    int lightingLevel; // 0:无 1:环境光 2.+漫反射, 3:+镜面光

    std::string texture;
    std::string bumpTexture;
    E_TextureFilterType texFilter;
    E_TextureAddressingMode texAddressMode;

    bool reflectible;
    bool refractable;
    float refractIndex;
    float _refractInvIndex;
    float reflectAndRefractWeight;
    float fresnelBias, fresnelPower;

    Material():
        ambientClr(Vector3::ZERO), diffuseClr(Vector3::ZERO), specularClr(Vector3::ZERO),
        power(0), lightingLevel(0), 
        reflectible(false), 
        refractable(false), refractIndex(0), _refractInvIndex(0),
        reflectAndRefractWeight(0),
        fresnelBias(0), fresnelPower(1),
        texFilter(ETFT_null), texAddressMode(ETAM_clamp){}
};

std::ostream& operator << (std::ostream& so, const Material& material);
std::istream& operator >> (std::istream& si, Material& material);

#endif // #ifndef MATERIAL_H
